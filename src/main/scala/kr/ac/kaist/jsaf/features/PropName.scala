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

  type t = (HashMap[Any, HashSet[Entity]], DisjointSets[Entity])

  override def featureName: String = "Property Name"
  private val number_literal: String = "$*Number*$"

  final private val useUniqueName = true // false: better recall and worse precision.
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

  var cachestr = mutable.HashMap[String, Entity]()

  def nid(id: Id): Entity = {
    val u =
      if (useUniqueName) id.getUniqueName.unwrap()
      else id.getText
//    if (debug) System.out.println("nid: "+u)
//    cachestr.get(u) match {
//      case Some(v) => v
//      case None =>
        val v = new Name(id.getText, u)
//        cachestr += u -> v
        v
//    }
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

  private def findarguments(node: Any): Boolean = {
    def findarguments_(node: Any): Boolean = {
      def rec(n: Any, e: Any) = (b: Boolean) => if (b) b else findarguments_(e)
      node match {
        case SNoOp(info, desc) => false
        case SStmtUnit(info, stmts) => findarguments_(stmts)
        case SFunDecl(info, ftn, strict) => false
        case SBlock(info, stmts, internal) => findarguments_(stmts)
        case SVarStmt(info, vds) => findarguments_(vds)
        case SEmptyStmt(info) => false
        case SExprStmt(info, expr, internal) => findarguments_(expr)
        case SIf(info, cond, trueBranch, falseBranch) =>
          findarguments_(cond) || findarguments_(trueBranch) || findarguments_(falseBranch)
        case SDoWhile(info, body, cond) =>
          findarguments_(body) || findarguments_(cond)
        case SWhile(info, cond, body) =>
          findarguments_(body) || findarguments_(cond)
        case SFor(info, init, cond, action, body) =>
          findarguments_(init) || findarguments_(cond) || findarguments_(action) || findarguments_(body)
        case SForIn(info, lhs, expr, body) =>
          findarguments_(lhs) || findarguments_(expr) || findarguments_(body)
        case SForVar(info, vars, cond, action, body) =>
          findarguments_(vars) || findarguments_(cond) || findarguments_(action) || findarguments_(body)
        case SForVarIn(info, _var, expr, body) =>
          findarguments_(_var) || findarguments_(expr) || findarguments_(body)
        case SContinue(info, target) => findarguments_(target)
        case SBreak(info, target) => findarguments_(target)
        case SReturn(info, expr) => findarguments_(expr)
        case SWith(info, expr, stmt) =>
          findarguments_(expr) || findarguments_(stmt)
        case SSwitch(info, cond, frontCases, _def, backCases) =>
          findarguments_(cond) || findarguments_(frontCases) || findarguments_(_def) || findarguments_(backCases)
        case SLabelStmt(info, label, stmt) =>
          findarguments_(label) || findarguments_(stmt)
        case SThrow(info, expr) =>
          findarguments_(expr)
        case STry(info, body, catchBlock, fin) =>
          findarguments_(body) || findarguments_(catchBlock) || findarguments_(fin)
        case SDebugger(info) => false
        case SSourceElements(info, body, strict) =>
          findarguments_(body)
        case SVarDecl(info, id, expr, strict) =>
          findarguments_(expr)
        case SCase(info, cond, body) =>
          findarguments_(cond) || findarguments_(body)
        case SCatch(info, id, body) =>
          findarguments_(body)
        case SExprList(info, exprs) =>
          findarguments_(exprs)
        case SCond(info, cond, trueBranch, falseBranch) =>
          findarguments_(cond) || findarguments_(trueBranch) || findarguments_(falseBranch)
        case SInfixOpApp(info, left, op, right) =>
          findarguments_(left) || findarguments_(right)
        case SPrefixOpApp(info, op, right) =>
          findarguments_(right)
        case SUnaryAssignOpApp(info, lhs, op) =>
          findarguments_(lhs)
        case SAssignOpApp(info, lhs, op, right) =>
          findarguments_(lhs) || findarguments_(right)
        case SThis(info) => false
        case SNull(info) => false
        case SBool(info, bool) => false
        case SDoubleLiteral(info, text, num) => false
        case SIntLiteral(info, intVal, radix) => false
        case SStringLiteral(info, quote, escaped) => false
        case SRegularExpression(info, body, flag) => false
        case SVarRef(info, id) if id.getText == "arguments" => true
        case SVarRef(info, id) => false
        case SArrayExpr(info, elements) => findarguments_(elements)
        case SArrayNumberExpr(info, elements) => findarguments_(elements)
        case SObjectExpr(info, members) => findarguments_(members)
        case SParenthesized(info, expr) => findarguments_(expr)
        case SFunExpr(info, ftn) => false
        case SBracket(info, obj, index) =>
          findarguments_(obj) || findarguments_(index)
        case SDot(info, obj, member) =>
          findarguments_(obj) || findarguments_(member)
        case SNew(info, lhs) => findarguments_(lhs)
        case SFunApp(info, fun, args) =>
          findarguments_(fun) || findarguments_(args)
        case SPropId(info, id) => false
        case SPropStr(info, str) => false
        case SPropNum(info, num) => false
        case SField(info, prop, expr) => findarguments_(expr)
        case SGetProp(info, prop, ftn) => false
        case SSetProp(info, prop, ftn) => false
        case SId(info, text, uniqueName, _with) if text == "arguments" => true
        case SId(info, text, uniqueName, _with) => false
        case SOp(info, text) => false
        case SLabel(info, id) => false
        case SComment(info, comment) => false
        case list: List[_] =>
          list.foldLeft(false)((b, node) => b || findarguments_(node))
        case Some(n) =>
          findarguments_(n)
        case None => false
      }
    }
    argCache.get(node) match {
      case Some(b) => b
      case None =>
        val n = findarguments_(node)
        argCache = argCache + (node -> n)
        n
    }

  }

  var argCache = HashMap[Any, Boolean]()

  def nofargs(node: Any): Int = {
    node match {
      case SFunDecl(_, SFunctional(_, _, body, _, list), _) if findarguments(body) => 1000
      case SFunDecl(_, SFunctional(_, _, _, _, list), _) => list.size
      case SFunExpr(_, SFunctional(_, _, body, _, list)) if findarguments(body) => 1000
      case SFunExpr(_, SFunctional(_, _, body, _, list)) => list.size
      case SNew(_, lhs) => nofargs(lhs)
      case SFunApp(_, _, list) => list.size
      case _ => 0
    }
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
//            System.out.println(n_lhs+" =:= "+n_rhs)
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
//            System.out.println(prop_name+" =:= "+n_rhs)
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

  private val cache = mutable.HashMap[Any, HashSet[Name]]()
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
//    if (debug) System.out.println("name: "+n)
    def name_(n: Any): HashSet[Entity] = {
//      if (debug) System.out.println("name_: "+n)
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
        case SVarRef(_, id) =>
//          if (debug) System.out.println("VarRef: "+id)
          HashSet(nid(id))
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

//    cache.get(n) match {
//      case Some(v) => v
//      case None =>
        val v = name_(n)
//        cache += (n -> v)
        v
//    }
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
    var name_binding = HashMap[Any, HashSet[Entity]]()
    val binding = new DisjointSets[Entity]()

    if (unification) {
      // inter-procedural binding
      cg.foreach(f => {
        val call_args =
          f._1 match {
            case SFunApp(_, _, args) => args
            case SNew(_, SFunApp(_, _, args)) => args
            case _ => Nil
          }
        f._2.foreach {
          case SFunExpr(_, SFunctional(_, _, body, _, params)) =>
            // argument binding
            listMerge(call_args, params).foreach(f => {
              val e = f._1
              val id = nid(f._2)

              // function <-> argument
              val funs = collectFuns(e)
              funs.foreach(f => {
                val nset = name_binding.getOrElse(f, HashSet[Entity]()) + id
                name_binding = name_binding + (f -> nset)
              })

              // id <-> argument
              val names_rhs = nameOfExpr(e).filter(p => !p.isEmpty)
              names_rhs.foreach(n_rhs => {
//                System.out.println(id + " =:= " + n_rhs)
                binding.union(id, n_rhs)
              })
            })
            // return value binding
            val returnExprs = collectReturns(body)
            // function <-> lhs
            returnExprs.foreach(e => {
              val funs = collectFuns(e)
              // * returned funs
              // * returned identifiers
              // call => returned funs
              // call => funs binded by the returned identifiers
              // binds lhs with the returned funs
              // binds lhs with the returned identifiers.
            })
        }
      })
    }

    val v = walkAST(collectFunExprName, after)(null, pgm)((name_binding, binding))

    binding.groups.foreach(System.out.println)

    v
  }

  var debug: Boolean = true

  def genFeature(maps: t)(map: FeatureMap) = {
    genFeatureInit()
    val nameMap = maps._1
    val tbl = maps._2

    val m = map.map(f => {
      val dc = f._1
      val span = kr.ac.kaist.jsaf.string(dc._2)
//      if (!span.startsWith("start")) debug = false
//      else debug = true

      val vectors = f._2
      val callname = dc._2 match {
        case SFunApp(_, call, _) =>
//          if (debug) System.out.println(call)
          name(call)
        case SNew(_, SFunApp(_, call, _)) => name(call)
        case SNew(_, lhs) => name(lhs)
      }

//      System.out.println(span+"callname: "+callname)
      val vec =
        nameMap.get(dc._1) match {
          case Some(xs) =>
//            System.out.println("declname: "+xs)
            val nac = nofargs(dc._2)
            val nae = nofargs(dc._1)
            val bargs = nac <= nae
            val b = xs.exists(x => {
              val xr = tbl.find(x)
              callname.exists(y => {
                val yr = tbl.find(y)
                if (debug) {
//                  System.out.println(span+" * "+yr+"("+nac+") ==>  "+xr+"("+nae+")  :   "+y+" -> "+x)
                }
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
