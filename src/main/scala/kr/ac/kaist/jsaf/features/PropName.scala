package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes.{Expr, Id}
import kr.ac.kaist.jsaf.scala_src.nodes._
import kr.ac.kaist.jsaf.utils.DisjointSets

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/**
 * Created by ysko on 15. 7. 23..
 */
object PropName extends Features {
  type t = (HashMap[Any, HashSet[Entity]], DisjointSets[Entity])

  override def featureName: String = "Property Name"
  private val number_literal: String = "$*Number*$"

  final private val useUniqueName = true // false: better recall and worse precision.
  final private val unification = true // true: better recall and worse precision.

  sealed trait Entity {
    val id: Int
    def comp(that: Entity): Int
    def comptext(that: Entity): Boolean

    def isEmpty: Boolean
  }
  case class Name(_text: String, _uniqueID: String) extends Entity {
    val id = 1
    val text = _text
    val uniqueID = _uniqueID
    def comptext(that: Entity): Boolean = {
      if (this.id - that.id != 0) false
      else this.text.equals(that.asInstanceOf[Name].text)
    }
    def comp(that: Entity): Int = {
      val oid = this.id - that.id
      if (oid != 0) oid
      else this.uniqueID compare that.asInstanceOf[Name].uniqueID
    }

    def isEmpty: Boolean = text == null || text.equalsIgnoreCase("")

    override def toString: String = _uniqueID
  }
  implicit object order_entity extends Ordering[Entity] {
    override def compare(x: Entity, y: Entity): Int = x.comp(y)
  }

  private val empty_funs = HashSet[Any]()
  private def collectFuns(e: Any): HashSet[Any] = {
    e match {
      case SExprList(_, list) if list.nonEmpty => collectFuns(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, et, ef) => collectFuns(et) ++ collectFuns(ef)
      case SInfixOpApp(_, el, _, er) => collectFuns(el) ++ collectFuns(er)
      case SPrefixOpApp(_, _, _) => empty_funs
      case SUnaryAssignOpApp(_, _, _) => empty_funs
      case SAssignOpApp(_, _, _, e1) => collectFuns(e1)
      case SLiteral(_) => empty_funs
      case SVarRef(_, _) => empty_funs
      case SArrayExpr(_, _) => empty_funs
      case SObjectExpr(_, _) => empty_funs
      case SParenthesized(_, e1) => collectFuns(e1)
      case SFunExpr(_, _) => HashSet(e)
      case SBracket(_, _, _) => empty_funs
      case SDot(_, _, _) => empty_funs
      case SNew(_, _) => empty_funs
      case SFunApp(_, _, _) => empty_funs
    }
  }

  var cachestr = mutable.HashMap[String, Entity]()

  def nid(id: Id): Entity = {
    val u =
      if (useUniqueName) id.getUniqueName.unwrap()
      else id.getText
    new Name(id.getText, u)
  }

  def nid(id: String): Entity = {
    cachestr.get(id) match {
      case Some(v) => v
      case None =>
        val v = new Name(id, id)
        cachestr += id -> v
        v
    }
  }

  // Find appropriate name string from 'lhs'
  private def nameOfExpr(lhs: Any): HashSet[Entity] = {
    lhs match {
      case SExprList(_, list) => nameOfExpr(list.last)
      case SCond(_, _, et, ef) => nameOfExpr(et) ++ nameOfExpr(ef)
      case SInfixOpApp(_, el, op, er) if op.getText.equals("||") => nameOfExpr(el) ++ nameOfExpr(er)
      case SInfixOpApp(_, el, _, er) => empty
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, _, _, e1) => nameOfExpr(e1)
      case SLiteral(_) => empty
      case SVarRef(_, id) => HashSet(nid(id))
      case SArrayExpr(_, list) => empty
      case SObjectExpr(_, list) => empty
      case SParenthesized(_, e) => nameOfExpr(e)
      case SFunExpr(_, _) => empty
      case SBracket(_, _, e) => value(e)
      case SDot(_, _, id) => HashSet(nid(id.getText))
      case SNew(_, _) => empty
      case SFunApp(_, _, _) => empty
    }
  }

  // Get a property string from 'pr'
  private def nameOfProp(pr: Any): Entity = pr match {
    case SPropId(_, id) => nid(id.getText)
    case SPropStr(_, str) => nid(str)
    case SPropNum(_, _) => nid(number_literal)
  }

  private def nameOfFun(fun: Any): Entity = fun match {
    case SFunExpr(_, SFunctional(_, _, _, name, _)) => nid(name)
  }

  var stack: List[Any] = List(0)
  def push(i: Any) = {
    stack = i::stack
  }
  def pop() = stack match {
    case i::rest =>
      stack = rest
      i
    case _ => throw new InternalError("empty stack")
  }
  def current = stack match {
    case i::rest => i
    case _ => throw new InternalError("empty stack")
  }

  private def collectFunExprName(parent: Any, node: Any, maps: t): t = {
    val map = maps._1
    val tbl = maps._2
    node match {
      case SFunDecl(_, f, _) =>
        push(node)
        val name = nid(f.getName)

        val i = (map.getOrElse(node, empty) + name).filter(p => !p.isEmpty)
        (map + (node -> i), tbl)
      case SFunExpr(_, f) =>
        push(node)
        val name = nid(f.getName)

        val i = (map.getOrElse(node, empty) + name).filter(p => !p.isEmpty)
        (map + (node -> i), tbl)

      case SAssignOpApp(_, lhs, SOp(_, "="), expr) =>
        val funs = collectFuns(expr)
        val names_lhs = nameOfExpr(lhs).filter(p => !p.isEmpty)
        val name = names_lhs ++ funs.map(nameOfFun)
        val map_2 =
          (map /: funs)((m, f) => {
            val i = (m.getOrElse(f, empty) ++ name).filter(p => !p.isEmpty)
            if (i.nonEmpty) m + (f -> i)
            else m
          })
        if (unification) {
          val names_rhs = nameOfExpr(expr).filter(p => !p.isEmpty)

          names_lhs.foreach(n_lhs => names_rhs.foreach(n_rhs => {
            tbl.union(n_lhs, n_rhs)
          }))
        }

        (map_2, tbl)
      case SField(_, prop, expr) =>
        val funs = collectFuns(expr)
        val prop_name = nameOfProp(prop)
        val name = funs.map(nameOfFun) + prop_name
        val map_2 =
          (map /: funs)((m, f) => {
            val i: HashSet[Entity] = (m.getOrElse(f, empty) ++ name).filter(p => !p.isEmpty)
            if (i.nonEmpty) m + (f -> i)
            else m
          })
        if (unification) {
          val names_rhs = nameOfExpr(expr).filter(p => !p.isEmpty)
          names_rhs.foreach(n_rhs => {
            tbl.union(prop_name, n_rhs)
          })
        }
        (map_2, tbl)
      case _ =>
        maps
    }
  }
  private def after(parent: Any, node: Any)(map: t): t = node match {
    case SFunDecl(_, _, _) =>
      pop()
      map
    case SFunExpr(_, _) =>
      pop()
      map
    case _ =>
      map
  }

  private val empty = HashSet[Entity]()

  private def value(e: Any): HashSet[Entity] = {
    e match {
      case SExprList(_, list) if list.nonEmpty => value(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, et, ef) => value(et) ++ value(ef)
      case SInfixOpApp(_, e1, _, e2) => value(e1) ++ value(e2) // TODO
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, _, _, e1) => value(e1) // e[x="10"] = 10;
      // lhs
      case SStringLiteral(_, _, s) => HashSet(nid(s))
      case SIntLiteral(_, i, _) => HashSet(nid(number_literal))
      case SDoubleLiteral(_, s, _) => HashSet(nid(number_literal))
      case SBool(_, tf) => HashSet(nid(tf.toString))
      case SNull(_) => HashSet(nid("null"))
      case SLiteral(_) => empty // regexp and this are ignored.
      case SVarRef(_, _) => empty
      case SArrayExpr(_, _) => empty
      case SObjectExpr(_, _) => empty
      case SParenthesized(_, e_i) => value(e_i)
      case SFunExpr(_, _) => empty
      case SBracket(_, _, _) => empty
      case SDot(_, _, _) => empty
      case SNew(_, _) => empty
      case SFunApp(_, _, _) => empty
    }
  }

  private def name(n: Any): HashSet[Entity] = {
    def name_(n: Any): HashSet[Entity] = {
      n match {
        case SFunExpr(_, _) => empty
        case SExprList(_, list) if list.nonEmpty => name_(list.last)
        case SExprList(_, _) => throw new InternalError("impossible case")
        case SCond(_, _, e1, e2) => name_(e1) ++ name_(e2)
        case SInfixOpApp(_, e1, _, e2) => name_(e1) ++ name_(e2)
        case SPrefixOpApp(_, _, _) => empty
        case SUnaryAssignOpApp(_, _, _) => empty
        case SAssignOpApp(_, lhs, _, e1) => name_(e1)
        case SThis(_) => empty
        case SVarRef(_, id) => HashSet(nid(id))
        case SArrayExpr(_, _) => empty
        case SArrayNumberExpr(_, _) => empty
        case SObjectExpr(_, _) => empty
        case SParenthesized(_, e) => name_(e)
        case SBracket(_, _, e) => value(e)
        case SDot(_, _, id) => HashSet(nid(id.getText))
        case SNew(_, lhs) => empty
        case SFunApp(_, call, args) => empty
      }
    }

    name_(n)
  }

  def collectReturns(e: Any): HashSet[Expr] = HashSet()

  private def listMerge[A,B](x: List[A], y: List[B]): List[(A, B)] = {
    (x, y) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (xh::xt, yh::yt) => (xh, yh)::listMerge(xt, yt)
    }
  }

  def init(pgm: Any, cg: OneshotCall.t): t = {
    val name_binding = HashMap[Any, HashSet[Entity]]()
    val binding = new DisjointSets[Entity]()

    val v = walkAST(collectFunExprName, after)(null, pgm)((name_binding, binding))

    v
  }

  var debug: Boolean = true

  def genFeature(maps: t)(map: FeatureMap) = {
    genFeatureInit()
    val nameMap = maps._1
    val tbl = maps._2

    val m = map.map(f => {
      val dc = f._1
      val callname = dc._2 match {
        case SFunApp(_, call, _) => name(call)
        case SNew(_, SFunApp(_, call, _)) => name(call)
        case SNew(_, lhs) => name(lhs)
      }

      val vec =
        nameMap.get(dc._1) match {
          case Some(xs) =>
            val b = xs.exists(x => {
              val xr = tbl.find(x)
              callname.exists(y => {
                val yr = tbl.find(y)
                (xr, yr) match {
                  case (Some(xx), Some(yy)) => xx.comp(yy) == 0 || x.comptext(y)
                  case _ => x.comptext(y)
                }
              })
            })
            if (b) 1
            else 0
          case _ => 0
        }

      (dc, vec)
    })

    val c = (HashMap[Any, Int]() /: m)((c_i, i) => {
      i._2 match {
        case 1 => c_i + (i._1._2 -> (c_i.getOrElse(i._1._2, 0) + 1))
        case _ => c_i
      }
    })

    val m_2 = map.map(f => {
      val dc = f._1
      val vectors = f._2
      c.get(dc._2) match {
        case Some(v) if v > 1 =>
          (dc, 0::vectors)
        case _ => (dc, m(dc)::vectors)
      }
    })

    genFeatureFinish()

    m_2
  }
}
