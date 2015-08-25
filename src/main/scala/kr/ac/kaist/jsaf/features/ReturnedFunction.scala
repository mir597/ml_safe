package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes.ASTNode
import kr.ac.kaist.jsaf.nodes_util.JSAstToConcrete
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable.{HashMap => MHashMap}
import kr.ac.kaist.jsaf.walkAST

/**
 * Created by ysko on 15. 8. 3.
 */
object ReturnedFunction extends Features {
  override def featureName: String = "Returned function calls"
  type t = (HashMap[Any, HashSet[String]], HashMap[Any, Any])
  val tv = (HashMap[Any, HashSet[String]](), HashMap[Any, Any]())

  private val number_literal = "$*Number*$"

  final private val useUniqueName = false // false: better recall and worse precision.

  private val empty_funs = HashSet[Any]()
  private def collectFuns(e: Any, f: Any): HashSet[Any] = {
    e match {
      case SExprList(_, list) if list.nonEmpty => collectFuns(list.last, f)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, et, ef) => collectFuns(et, f) ++ collectFuns(ef, f)
      case SInfixOpApp(_, el, _, er) => collectFuns(el, f) ++ collectFuns(er, f)
      case SPrefixOpApp(_, _, _) => empty_funs
      case SUnaryAssignOpApp(_, _, _) => empty_funs
      case SAssignOpApp(_, _, _, e1) => collectFuns(e1, f)
      case SLiteral(_) => empty_funs
      case SVarRef(_, _) => empty_funs
      case SArrayExpr(_, _) => empty_funs
      case SObjectExpr(_, _) => empty_funs
      case SParenthesized(_, e1) => collectFuns(e1, f)
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
    val nmap = maps._2
    node match {
      case SFunDecl(_, f, _) =>
        push(node)
        val name =
          if (useUniqueName) f.getName.getUniqueName.unwrap()
          else f.getName.getText

        val i = map.getOrElse(node, empty) + name
        (map + (node -> i), nmap)
      case SFunExpr(_, f) =>
        push(node)
        val name =
          if (useUniqueName) f.getName.getUniqueName.unwrap()
          else f.getName.getText

        if (name != null) {
          val i = map.getOrElse(node, empty) + name
          (map + (node -> i), nmap)
        } else maps

      case SReturn(_, e) if e.isDefined =>
        val expr = e.get
        val f = current
        val funs = collectFuns(expr, f)
        val nmap_2 =
          (nmap /: funs)((m, f_returned) => {
            m + (f_returned -> f)
          })

        (map, nmap_2)

      case SAssignOpApp(_, lhs, SOp(_, "="), expr) =>
        val f = current
        val funs = collectFuns(expr, f)
        val name = nameOfLHS(lhs) ++ funs.map(nameOfFun)
        ((map /: funs)((m, f) => {
          val i = m.getOrElse(f, empty) ++ name
          if (i.nonEmpty) m + (f -> i)
          else m
        }), nmap)
      case SField(_, prop, expr) =>
        val f = current
        val funs = collectFuns(expr, f)
        val name = funs.map(nameOfFun) + nameOfProp(prop)
        ((map /: funs)((m, f) => {
          val i = m.getOrElse(f, empty) ++ name
          if (i.nonEmpty) m + (f -> i)
          else m
        }), nmap)
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

  private val cache = MHashMap[Any, HashSet[String]]()
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

  private def exprd(e: Any): HashSet[String] = {
    e match {
      case SFunExpr(_, _) => empty
      case SExprList(_, list) if list.nonEmpty => exprd(list.last)
      case SExprList(_, _) => throw new InternalError("impossible case")
      case SCond(_, _, e1, e2) => exprd(e1) ++ exprd(e2)
      case SInfixOpApp(_, e1, _, e2) => exprd(e1) ++ exprd(e2)
      case SPrefixOpApp(_, _, _) => empty
      case SUnaryAssignOpApp(_, _, _) => empty
      case SAssignOpApp(_, lhs, _, e1) => exprd(e1) ++ named(lhs)
      case _ => named(e)
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

  private def named(n: Any): HashSet[String] = {
    def name_(n: Any): HashSet[String] = {
      n match {
        case SThis(_) => empty
        case SVarRef(_, id) if useUniqueName => HashSet(id.getUniqueName.unwrap())
        case SVarRef(_, id) if !useUniqueName => HashSet(id.getText)
        case SArrayExpr(_, _) => empty
        case SArrayNumberExpr(_, _) => empty
        case SObjectExpr(_, _) => empty
        case SParenthesized(_, e) => exprd(e)
        case SFunExpr(_, _) => empty
        case SBracket(_, _, e) => empty
        case SDot(_, _, id) => empty
        case SNew(_, lhs) => empty
        case SFunApp(_, call, args) => named(call)
      }
    }
    name_(n)
  }

  private def name(n: Any): HashSet[String] = {
    def name_(n: Any): HashSet[String] = {
      n match {
        case SThis(_) => empty
        case SVarRef(_, id) => empty
        case SArrayExpr(_, _) => empty
        case SArrayNumberExpr(_, _) => empty
        case SObjectExpr(_, _) => empty
        case SParenthesized(_, e) => expr(e)
        case SFunExpr(_, _) => empty
        case SBracket(_, _, e) => empty
        case SDot(_, _, id) => empty
        case SNew(_, lhs) => empty
        case SFunApp(_, call, args) => named(call)
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

  def init(pgm: Any): t = walkAST(collectFunExprName, after)(null, pgm)(tv)

  def genFeature(nameMap: t)(map: FeatureMap) = {
    genFeatureInit()

    val nm = nameMap._1
    val fm = nameMap._2

    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2

      val callname = dc._2 match {
        case SFunApp(_, call, _) => name(call)
        case SNew(_, SFunApp(_, call, _)) => name(call)
        case SNew(_, lhs) => name(lhs)
      }


      val vec =
        fm.get(dc._1) match {
          case Some(x) =>
            nm.get(x) match {
              case Some(xs) =>
                if (xs.intersect(callname).nonEmpty) 1
                else 0
              case _ => 0
            }
          case _ => 0
        }

      (dc, vec::vectors)
    })

    genFeatureFinish()

    m
  }
}
