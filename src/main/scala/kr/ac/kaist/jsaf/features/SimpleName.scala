package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes.Id
import kr.ac.kaist.jsaf.scala_src.nodes._
import kr.ac.kaist.jsaf.walkAST

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/**
 * Created by ysko on 15. 7. 23..
 */
object SimpleName {
  type t = HashMap[Any, Entity]

  private val number_literal: String = "$*Number*$"

  final private val useUniqueName = true // false: better recall and worse precision.

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

  val cachestr = mutable.HashMap[String, Entity]()

  def nid(id: Id): Entity = {
    val u =
      if (useUniqueName) id.getUniqueName.unwrap()
      else id.getText
//    new Name(u)
    u
  }

  def nid(id: String): Entity = {
    cachestr.get(id) match {
      case Some(v) => v
      case None =>
//        val v = new Name(id)
        val v = id
        cachestr += id -> v
        v
    }
  }

  // Find appropriate name string from 'lhs'
  def nameOfExpr(lhs: Any): Option[Entity] = {
    lhs match {
      case SExprList(_, list) => nameOfExpr(list.last)
      case SCond(_, _, et, ef) => None
      case SInfixOpApp(_, el, op, er) => None
      case SPrefixOpApp(_, _, _) => None
      case SUnaryAssignOpApp(_, _, _) => None
      case SAssignOpApp(_, _, _, e1) => nameOfExpr(e1)
      case SLiteral(_) => None
      case SVarRef(_, id) => Some(nid(id))
      case SArrayExpr(_, list) => None
      case SObjectExpr(_, list) => None
      case SParenthesized(_, e) => nameOfExpr(e)
      case SFunExpr(_, _) => None
      case SBracket(_, _, e) => value(e)
      case SDot(_, _, id) => Some(nid(id.getText))
      case SNew(_, _) => None
      case SFunApp(_, _, _) => None
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

  private def collectFunExprName(parent: Any, node: Any, maps: t): t = {
    val map = maps
    node match {
      case SAssignOpApp(_, lhs, SOp(_, "="), expr) =>
        val funs = collectFuns(expr)
        val names_lhs = nameOfExpr(lhs)
        val name = names_lhs
        val map_2 =
          (map /: funs)((m, f) => {
            name match {
              case Some(v) => m + (f -> v)
              case None => m
            }
          })
        map_2
//      case SField(_, prop, expr) =>
//        val funs = collectFuns(expr)
//        val prop_name = nameOfProp(prop)
//        val name = funs.map(nameOfFun) + prop_name
//        val map_2 =
//          (map /: funs)((m, f) => {
//            val i: HashSet[Entity] = (m.getOrElse(f, empty) ++ name).filter(p => !p.isEmpty)
//            if (i.nonEmpty) m + (f -> i)
//            else m
//          })
//        map_2
      case _ =>
        maps
    }
  }

  private val empty = None

  private def value(e: Any): Option[Entity] = {
    e match {
      case SExprList(_, list) if list.nonEmpty => value(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, et, ef) => None
      case SInfixOpApp(_, e1, _, e2) => None
      case SPrefixOpApp(_, _, _) => None
      case SUnaryAssignOpApp(_, _, _) => None
      case SAssignOpApp(_, _, _, e1) => value(e1) // e[x="10"] = 10;
      // lhs
      case SStringLiteral(_, _, s) => Some(nid(s))
      case SIntLiteral(_, i, _) => Some(nid(number_literal))
      case SDoubleLiteral(_, s, _) => Some(nid(number_literal))
      case SBool(_, tf) => Some(nid(tf.toString))
      case SNull(_) => Some(nid("null"))
      case SLiteral(_) => None // regexp and this are ignored.
      case SVarRef(_, _) => None
      case SArrayExpr(_, _) => None
      case SObjectExpr(_, _) => None
      case SParenthesized(_, e_i) => value(e_i)
      case SFunExpr(_, _) => None
      case SBracket(_, _, _) => None
      case SDot(_, _, _) => None
      case SNew(_, _) => None
      case SFunApp(_, _, _) => None
    }
  }

  def callname(n: Any): Option[Entity] = {
    n match {
      case SFunApp(_, call, _) => name(call)
      case SNew(_, SFunApp(_, call, _)) => name(call)
      case SNew(_, lhs) => name(lhs)
    }
  }

  def name(n: Any): Option[Entity] = {
    def name_(n: Any): Option[Entity] = {
      n match {
        case SFunExpr(_, _) => None
        case SExprList(_, list) if list.nonEmpty => name_(list.last)
        case SExprList(_, _) => throw new InternalError("impossible case")
        case SCond(_, _, e1, e2) => None
        case SInfixOpApp(_, e1, _, e2) => None
        case SPrefixOpApp(_, _, _) => empty
        case SUnaryAssignOpApp(_, _, _) => empty
        case SAssignOpApp(_, lhs, _, e1) => name_(e1)
        case SThis(_) => empty
        case SVarRef(_, id) => Some(nid(id))
        case SArrayExpr(_, _) => empty
        case SArrayNumberExpr(_, _) => empty
        case SObjectExpr(_, _) => empty
        case SParenthesized(_, e) => name_(e)
        case SBracket(_, _, e) => value(e)
        case SDot(_, _, id) => Some(nid(id.getText))
        case SNew(_, lhs) => empty
        case SFunApp(_, call, args) => empty
      }
    }

    name_(n)
  }

  def declFeature(pgm: Any): t = {
    val name_binding = HashMap[Any, Entity]()

    val v = walkAST(collectFunExprName)(null, pgm)(name_binding)

    v
  }

  var debug: Boolean = true
}
