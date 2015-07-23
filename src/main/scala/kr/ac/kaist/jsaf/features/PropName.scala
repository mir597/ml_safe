package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.{HashSet, HashMap}

/**
 * Created by ysko on 15. 7. 23..
 */
object PropName {
  type t = HashMap[Any, HashSet[String]]

  // Collects function expressions from 'node'
  private def collectFuns(parent: Any, node: Any, set: HashSet[Any]) = {
    node match {
      case SFunExpr(_, ftn) => set + node
      case _ => set
    }
  }

  // Find appropriate name string from 'lhs'
  private def nameOfLHS(parent: Any, lhs: Any, name: Option[String]) = lhs match {
    case SVarRef(_, id) => Some(id.getText)
    case SDot(_, _, id) => Some(id.getText)
    case SStringLiteral(_, qs, es) => Some(es)
    case _ => name
  }
  // Get a property string from 'pr'
  private def nameOfProp(pr: Any): String = pr match {
    case SPropId(_, id) => id.getText
    case SPropStr(_, str) => str
    case SPropNum(_, n) =>
      // FIXME How can I get the number literal?
      System.err.println("PropNumLiteral: "+n)
      "__Number__"
  }

  private def collectFunExprName(parent: Any, node: Any, map: HashMap[Any, HashSet[String]]) = {
    node match {
      case SAssignOpApp(info, lhs, op, expr) =>
        val funs = walkAST(collectFuns)(null, expr)(HashSet())
        if (funs.nonEmpty) {
          val name = walkAST(nameOfLHS)(null, lhs)(None)
          name match {
            case Some(n) =>
              funs.foldLeft(map)((m, f) => {
                val i = m.getOrElse(f, HashSet[String]()) + n
                m + (f -> i)
              })
            case None => map
          }
        } else {
          map
        }
      case SField(_, prop, expr) =>
        val funs = walkAST(collectFuns)(null, expr)(HashSet())
        if (funs.nonEmpty) {
          val name = nameOfProp(prop)
          funs.foldLeft(map)((m, f) => {
            val i = m.getOrElse(f, HashSet[String]()) + name
            m + (f -> i)
          })
        } else {
          map
        }
      case _ => map
    }
  }

  def init(pgm: Any): t = walkAST(collectFunExprName)(null, pgm)(HashMap[Any, HashSet[String]]())

  def genFeature(nameMap: t)(map: HashMap[(Any, Any), List[Int]]) = {
    map.map(f => {
      val dc = f._1
      val vectors = f._2
      val callname = name(dc._2)

      val vec =
        nameMap.get(dc._1) match {
          case Some(xs) =>
            if (xs.contains(callname)) 1
            else 0
          case None => 0
        }

      (dc, vec::vectors)
    })
  }
}
