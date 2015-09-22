package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes._
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.{HashSet, HashMap}
import kr.ac.kaist.jsaf.walkAST

/**
 * Created by ysko on 15. 8. 3.
 */
object OneshotCall extends Features {
  override def featureName: String = "One-shot call"
  type t = HashMap[Any, HashSet[Any]]

  val empty = HashSet[Any]()

  def decls(node: Any): HashSet[Any] = {
    node match {
      case SNoOp(info, desc) => empty
      case SStmtUnit(info, stmts) => decls(stmts)
      case SBlock(info, stmts, internal) => decls(stmts)
      case SVarStmt(info, vds) => decls(vds)
      case SEmptyStmt(info) => empty
      case SExprStmt(info, expr, internal) => decls(expr)
      case SIf(info, cond, trueBranch, falseBranch) =>
        decls(trueBranch) ++
          decls(falseBranch)
      case SDoWhile(info, body, cond) => decls(body)
      case SWhile(info, cond, body) => decls(body)
      case SContinue(info, target) => empty
      case SBreak(info, target) => empty
      case SReturn(info, expr) => empty
      case SWith(info, expr, stmt) =>
        decls(stmt)
      case SSwitch(info, cond, frontCases, _def, backCases) =>
        decls(frontCases) ++
          decls(_def) ++
          decls(backCases)
      case SLabelStmt(info, label, stmt) =>
        decls(stmt)
      case SThrow(info, expr) =>
        decls(expr)
      case STry(info, body, catchBlock, fin) =>
        decls(body) ++
          decls(catchBlock) ++
          decls(fin)
      case SDebugger(info) => empty
      case SSourceElements(info, body, strict) => decls(body)
      case SVarDecl(info, id, expr, strict) => decls(expr)
      case SCase(info, cond, body) => decls(body)
      case SCatch(info, id, body) => decls(body)
      case SExprList(info, exprs) => decls(exprs)
      case SCond(info, cond, trueBranch, falseBranch) => decls(trueBranch) ++ decls(falseBranch)
      case SInfixOpApp(info, left, op, right) => decls(left) ++ decls(right)
      case SPrefixOpApp(info, op, right) => decls(right)
      case SUnaryAssignOpApp(info, lhs, op) => empty
      case SAssignOpApp(info, lhs, op, right) => decls(lhs) ++ decls(right)
      case SThis(info) => empty
      case SNull(info) => empty
      case SBool(info, bool) => empty
      case SDoubleLiteral(info, text, num) => empty
      case SIntLiteral(info, intVal, radix) => empty
      case SStringLiteral(info, quote, escaped) => empty
      case SRegularExpression(info, body, flag) => empty
      case SVarRef(info, id) => empty
      case SArrayExpr(info, elements) => decls(elements)
      case SArrayNumberExpr(info, elements) => empty
      case SObjectExpr(info, members) => decls(members)
      case SParenthesized(info, expr) => decls(expr)

      case SFunExpr(info, ftn) => HashSet(node)

      case SBracket(info, obj, index) => empty
      case SDot(info, obj, member) => empty
      case SNew(info, lhs) =>
        // TODO (new Function('...'))()
        empty
      case SFunApp(info, fun, args) => empty
      case SPropId(info, id) => empty
      case SPropStr(info, str) => empty
      case SPropNum(info, num) => empty
      case SField(info, prop, expr) => empty
      case SGetProp(info, prop, ftn) => empty
      case SSetProp(info, prop, ftn) => empty
      case SId(info, text, uniqueName, _with) => empty
      case SOp(info, text) => empty
      case SLabel(info, id) => empty
      case SComment(info, comment) => empty
//      case STopLevel(fds, vds, stmts) => empty

      case list: List[_] => decls(list.last)
      case Some(n) => decls(n)
      case None => empty
    }
  }

  private def collectOneshotCalls(parent: Any, node: Any, map: HashMap[Any, HashSet[Any]]) = {
    node match {
      case SFunApp(info, fun, args) =>
        val set = decls(fun)
        if (set.nonEmpty) map + (node -> set)
        else map
      case SNew(_, lhs) =>
        val set = decls(lhs)
        if (set.nonEmpty) map + (node -> set)
        else map
      case _ => map
    }
  }

  def init(pgm: Any): t = walkAST(collectOneshotCalls)(null, pgm)(HashMap[Any, HashSet[Any]]())

  def feature(funMap: t)(decl: Any, call: Any): Int = {
    funMap.get(call) match {
      case Some(set) if set.contains(decl) => 1
      case _ => 0
    }
  }

  def genFeature(funMap: t)(map: FeatureMap) = {
    genFeatureInit()

    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2

      val vec = feature(funMap)(dc._1, dc._2)
      (dc, vec::vectors)
    })

    genFeatureFinish()

    m
  }
}
