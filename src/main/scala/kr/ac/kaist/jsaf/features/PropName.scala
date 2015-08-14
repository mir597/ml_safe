package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes.Id
import kr.ac.kaist.jsaf.scala_src.nodes._
import kr.ac.kaist.jsaf.utils.DisjointSets

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

/**
 * Created by ysko on 15. 7. 23..
 */
object PropName extends Features {
  class Name(_text: String, _uniqueID: String) {
    val text = _text
    val uniqueID = _uniqueID
    def comptext(that: Name): Boolean = this.text.equals(that.text)
    def comp(that: Name): Int = this.uniqueID compare that.uniqueID

    def isEmpty: Boolean = text == null || text.equalsIgnoreCase("")
  }
  implicit object order_name extends Ordering[Name] {
    override def compare(x: Name, y: Name): Int = x.comp(y)
  }

  type t = (HashMap[Any, HashSet[Name]], DisjointSets[Name])

  override def featureName: String = "Property Name"
  private val number_literal: String = "$*Number*$"

  final private val useUniqueName = false // false: better recall and worse precision.
  final private val unification = true // true: better recall and worse precision.

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

  var cachestr = mutable.HashMap[String, Name]()

  def nid(id: Id): Name = {
    val u =
      if (useUniqueName) id.getUniqueName.unwrap()
      else id.getText
    cachestr.get(u) match {
      case Some(v) => v
      case None =>
        val v = new Name(id.getText, u)
        cachestr += u -> v
        v
    }
  }

  def nid(id: String): Name = {
    cachestr.get(id) match {
      case Some(v) => v
      case None =>
        val v = new Name(id, id)
        cachestr += id -> v
        v
    }
  }

  // Find appropriate name string from 'lhs'
  private def nameOfExpr(lhs: Any): HashSet[Name] = {
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
  private def nameOfProp(pr: Any): Name = pr match {
    case SPropId(_, id) => nid(id.getText)
    case SPropStr(_, str) => nid(str)
    case SPropNum(_, _) => nid(number_literal)
  }

  private def nameOfFun(fun: Any): Name = fun match {
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

          names_lhs.foreach(n_lhs => names_rhs.foreach(n_rhs => tbl.union(n_lhs, n_rhs)))
        }

        (map_2, tbl)
      case SField(_, prop, expr) =>
        val funs = collectFuns(expr)
        val prop_name = nameOfProp(prop)
        val name = funs.map(nameOfFun) + prop_name
        val map_2 =
          (map /: funs)((m, f) => {
            val i = (m.getOrElse(f, empty) ++ name).filter(p => !p.isEmpty)
            if (i.nonEmpty) m + (f -> i)
            else m
          })
        if (unification) {
          val names_rhs = nameOfExpr(expr).filter(p => !p.isEmpty)
          names_rhs.foreach(n_rhs => tbl.union(prop_name, n_rhs))
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

  private val cache = mutable.HashMap[Any, HashSet[Name]]()
  private val empty = HashSet[Name]()

  private def value(e: Any): HashSet[Name] = {
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
      case SParenthesized(_, e) => value(e)
      case SFunExpr(_, _) => empty
      case SBracket(_, _, _) => empty
      case SDot(_, _, _) => empty
      case SNew(_, _) => empty
      case SFunApp(_, _, _) => empty
    }
  }

  private def name(n: Any): HashSet[Name] = {
    def name_(n: Any): HashSet[Name] = {
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

    cache.get(n) match {
      case Some(v) => v
      case None =>
        val v = name_(n)
        cache += (n -> v)
        v
    }
  }

  def init(pgm: Any): t = walkAST(collectFunExprName, after)(null, pgm)((HashMap[Any, HashSet[Name]](), new DisjointSets[Name]()))

  def genFeature(maps: t)(map: FeatureMap) = {
    genFeatureInit()
    val nameMap = maps._1
    val tbl = maps._2

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

      (dc, vec::vectors)
    })

    genFeatureFinish()

    m
  }
}
