package kr.ac.kaist.jsaf.utils

import scala.collection.immutable.TreeMap

object UnionFind {
  trait S {
    type T
    type Elt
    val empty: T
    def add(v: Elt, uf: T): T
    def get_representative(v: Elt, uf: T): (T, Elt)
    def union(e1: Elt, e2: Elt, t: T): T
    def mem(e: Elt, t: T): Boolean
    // fmt
    def fold[A](f: (A, (Elt, Elt)) => A, t: T, r: A): A
    def normalize(t: T): T
    def meet_raw(t1: T, t2: T): (T,T,T)
    def eqs(t: T): List[(Elt,Elt)]

    sealed trait RemoveResult
    case class Representative(e: Elt) extends RemoveResult
    case object NoRepresentative extends RemoveResult
    case object RepresentativeSame extends RemoveResult

    def remove(e: Elt, t: T): (T, RemoveResult)
    // TreeMap.merge
    def mapMerge[K,V1,V2](f: (K, Option[V1], Option[V1]) => Option[V2], uf1: TreeMap[K,V1], uf2: TreeMap[K,V1])(implicit ord: Ordering[K]) = {
      val keys = uf1.keySet ++ uf2.keySet
      keys.foldLeft(TreeMap[K,V2]())((m, k) => {
        val ov = f(k, uf1.get(k), uf2.get(k))
        ov match {
          case Some(v) => m + (k -> v)
          case None => m
        }
      })
    }
  }

  def Make[T_]()(implicit ord_cs: Ordering[T_]) = new S {
    override type Elt = T_
    override type T = TreeMap[Elt, Elt]

    override val empty = TreeMap[Elt, Elt]()

    override def add(v: Elt, uf: T): T = {
      if (uf.contains(v)) uf
      else uf + (v -> v)
    }

    override def get_representative(v: Elt, uf: T): (T, Elt) = {
      uf.get(v) match {
        case Some(rep) =>
          if (ord_cs.compare(v, rep) == 0) (uf, rep)
          else {
            val (uf_2, rep_2) = get_representative(rep, uf)
            (uf_2 + (v -> v), rep_2)
          }
        case None => (uf + (v -> v), v)
      }
    }

    override def union(v1: Elt, v2: Elt, uf: T): T = {
      val (uf_1, v1_rep) = get_representative(v1, uf)
      val (uf_2, v2_rep) = get_representative(v2, uf_1)
      if (ord_cs.compare(v1_rep, v2_rep) < 0)
        uf_2 + (v2_rep -> v1_rep)
      else
        uf_2 + (v1_rep -> v2_rep)
    }

    override def mem(v: Elt, uf: T): Boolean = uf.contains(v)

    // fmt

    override def fold[A](f: (A, (Elt, Elt)) => A, t: T, r: A): A = {
      t.foldLeft[A](r)(f)
    }

    implicit object ord_tt extends Ordering[(Elt, Elt)] {
      override def compare(x: (Elt, Elt), y: (Elt, Elt)): Int = {
        if (x == y) 0
        else {
          val res = ord_cs.compare(x._1, y._1)
          if (res == 0) ord_cs.compare(x._2, y._2)
          else res
        }
      }
    }

    override def normalize(uf: T): T = {
      uf.map(kv => {
        val (_, v) = get_representative(kv._1, uf)
        kv._1 -> v
      })
    }

    /* from http://www.cs.cmu.edu/~ab/Desktop/15-211%20Archive/res00035/Lecture26MRMS.ppt */
    override def meet_raw(uf1: T, uf2: T): (T, T, T) = {
      val uf1_ = normalize(uf1)
      val uf2_ = normalize(uf2)
      var memo = TreeMap[(Elt, Elt), Elt]()

      val res = mapMerge((v: Elt, r1: Option[Elt], r2: Option[Elt]) => {
        (r1, r2) match {
          case (Some(v1), Some(v2)) =>
            if (ord_cs.compare(v1, v2) == 0) r1
            else {
              val res_ = memo.get((v1, v2))
              res_ match {
                case Some(res__) => Some(res__)
                case None =>
                  memo += (v1, v2) -> v
                  Some(v)
              }
            }
          case (Some(_), None) => Some(v)
          case (None, Some(_)) => Some(v)
          case _ => throw new InternalError("impossible case")
        }
      }, uf1_, uf2_)
      (uf1_, uf2_, res)
    }

    override def eqs(uf: T): List[(Elt, Elt)] = {
      val m = normalize(uf)
      m.toList
    }

    override def remove(a: Elt, uf: T): (T, RemoveResult) = {
      val uf_ = normalize(uf)
      uf_.get(a) match {
        case Some(v) =>
          if (ord_cs.compare(v, a) == 0) {
            /* removing representative */
            val (uf_1, found) = uf.foldLeft[(T, Option[Elt])]((uf, None))((m, kv) => {
              val (k, v) = kv
              val (uf_2, found) = m
              found match {
                case Some(rep) if ord_cs.compare(v, a) == 0 =>
                  val uf_3 = uf_2 + (k -> rep)
                  (uf_3, found)
                case None if ord_cs.compare(v, a) == 0 && ord_cs.compare(k, a) != 0 =>
                  val uf_4 = uf_2 + (k -> k)
                  (uf_4, Some(k))
                case _ =>
                  (uf_2, found)
              }
            })
            val uf_5 = uf_1 - a
            found match {
              case None => (uf_5, NoRepresentative)
              case Some(v_) => (uf_5, Representative(v_))
            }
          } else {
            /* removing non-representative */
            (uf_ - a, RepresentativeSame)
          }
        case None =>
          // Not_found
          (uf_, RepresentativeSame)
      }
    }
  }
}
