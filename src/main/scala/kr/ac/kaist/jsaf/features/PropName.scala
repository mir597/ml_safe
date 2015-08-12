package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/**
 * Created by ysko on 15. 7. 23..
 */
object PropName extends Features {
  type t = HashMap[Any, HashSet[String]]

  override def featureName: String = "Property Name"
  private val number_literal = "$*Number*$"

  final private val useUniqueName = false // false: better recall and worse precision.

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

  private def nameOfExpr(e: Any): HashSet[String] = {
    e match {
      case SExprList(_, list) => nameOfExpr(list.last)
      case SCond(_, _, et, ef) => nameOfExpr(et) ++ nameOfExpr(ef)
      case SInfixOpApp(_, el, _, er) => nameOfExpr(el) ++ nameOfExpr(er)
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, _, _, e1) => nameOfExpr(e1)
      case _ => nameOfLHS(e)
    }
  }

  // Find appropriate name string from 'lhs'
  private def nameOfLHS(lhs: Any): HashSet[String] = {
    lhs match {
      case SLiteral(_) => empty
      case SVarRef(_, id) if useUniqueName => HashSet(id.getUniqueName.unwrap())
      case SVarRef(_, id) if !useUniqueName => HashSet(id.getText)
      case SArrayExpr(_, list) => empty
      case SObjectExpr(_, list) => empty
      case SParenthesized(_, e) => nameOfExpr(e)
      case SBracket(_, _, e) => value(e)
      case SDot(_, _, id) => HashSet(id.getText)
      case SFunApp(_, _, _) => empty
    }
  }

  // Get a property string from 'pr'
  private def nameOfProp(pr: Any): String = pr match {
    case SPropId(_, id) => id.getText
    case SPropStr(_, str) => str
    case SPropNum(_, n) => number_literal
  }

  private def nameOfFun(fun: Any): String = fun match {
    case SFunExpr(_, SFunctional(_, _, _, name, _)) if useUniqueName => name.getUniqueName.unwrap()
    case SFunExpr(_, SFunctional(_, _, _, name, _)) if !useUniqueName => name.getText
  }

  private def collectFunExprName(parent: Any, node: Any, map: HashMap[Any, HashSet[String]]) = {
    node match {
      case SFunDecl(_, f, _) =>
        val name =
          if (useUniqueName) f.getName.getUniqueName.unwrap()
          else f.getName.getText

        val i = map.getOrElse(node, empty) + name
        map + (node -> i)
      case SFunExpr(_, f) =>
        val name =
          if (useUniqueName) f.getName.getUniqueName.unwrap()
          else f.getName.getText

        if (name != null) {
          val i = map.getOrElse(node, empty) + name
          map + (node -> i)
        } else map

      case SAssignOpApp(_, lhs, SOp(_, "="), expr) =>
        val funs = collectFuns(expr)
        val name = nameOfLHS(lhs) ++ funs.map(nameOfFun)
        (map /: funs)((m, f) => {
          val i = m.getOrElse(f, empty) ++ name
          if (i.nonEmpty) m + (f -> i)
          else m
        })
      case SField(_, prop, expr) =>
        val funs = collectFuns(expr)
        val name = funs.map(nameOfFun) + nameOfProp(prop)
        (map /: funs)((m, f) => {
          val i = m.getOrElse(f, empty) ++ name
          if (i.nonEmpty) m + (f -> i)
          else m
        })
      case _ =>
        map
    }
  }

  private val cache = mutable.HashMap[Any, HashSet[String]]()
  private val empty = HashSet[String]()

  private def value(e: Any): HashSet[String] = {
    e match {
      case SExprList(_, list) if list.nonEmpty => value(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, et, ef) => value(et) ++ value(ef)
      case SInfixOpApp(_, e1, _, e2) => value(e1) ++ value(e2) // TODO
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, _, _, e1) => value(e1) // e[x="10"] = 10;
      // lhs
      case SStringLiteral(_, _, s) => HashSet(s)
      case SIntLiteral(_, i, _) => HashSet(number_literal)
      case SDoubleLiteral(_, s, _) => HashSet(number_literal)
      case SBool(_, tf) => HashSet(tf.toString)
      case SNull(_) => HashSet("null")
      case SLiteral(_) => empty // regexp and this are ignored.
      case SVarRef(_, _) => empty
      case SArrayExpr(_, _) => empty
      case SObjectExpr(_, _) => empty
      case SParenthesized(_, e) => value(e)
      case SFunExpr(_, _) => empty
      case SBracket(_, _, _) => empty
      case SDot(_, _, _) => empty
      case SNew(_, _) => empty
      case SFunApp(_, _, _) => empty
    }
  }

  private def expr(e: Any): HashSet[String] = {
    e match {
      case SFunExpr(_, _) => empty
      case SExprList(_, list) if list.nonEmpty => expr(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, e1, e2) => expr(e1) ++ expr(e2)
      case SInfixOpApp(_, e1, _, e2) => expr(e1) ++ expr(e2)
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, lhs, _, e1) => expr(e1) ++ name(lhs)
      case _ => name(e)
    }
  }

  private def name(n: Any): HashSet[String] = {
    def name_(n: Any): HashSet[String] = {
      n match {
        case SThis(_) => empty
        case SVarRef(_, id) if useUniqueName => HashSet(id.getUniqueName.unwrap())
        case SVarRef(_, id) if !useUniqueName => HashSet(id.getText)
        case SArrayExpr(_, _) => empty
        case SArrayNumberExpr(_, _) => empty
        case SObjectExpr(_, _) => empty
        case SParenthesized(_, e) => expr(e)
        case SFunExpr(_, _) => empty
        case SBracket(_, _, e) => value(e)
        case SDot(_, _, id) => HashSet(id.getText)
        case SNew(_, lhs) => empty
        case SFunApp(_, call, args) => empty
      }
    }

    cache.get(n) match {
      case Some(v) => v
      case None =>
        val v = name_(n)
        cache += (n -> v)
        v
    }
  }

  def init(pgm: Any): t = walkAST(collectFunExprName)(null, pgm)(HashMap[Any, HashSet[String]]())

  def genFeature(nameMap: t)(map: FeatureMap) = {
    genFeatureInit()

    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2
      val callname = dc._2 match {
        case SFunApp(_, call, _) => name(call)
        case SNew(_, SFunApp(_, call, _)) => name(call)
        case SNew(_, lhs) => name(lhs)
      }

      val vec =
        nameMap.get(dc._1) match {
          case Some(xs) =>
            if (xs.intersect(callname).nonEmpty) 1
            else 0
          case _ => 0
        }

      (dc, vec::vectors)
    })

    genFeatureFinish()

    m
  }
}
