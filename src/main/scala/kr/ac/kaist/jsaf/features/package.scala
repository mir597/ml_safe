package kr.ac.kaist.jsaf

import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.mutable

/**
 * Created by ysko on 15. 7. 23..
 */
package object features {

  val cache = mutable.HashMap[Any, String]()
  def name(n: Any): String = {
    def name_(n: Any): String = {
      n match {
        case SFunDecl(_, fun, _) => fun.getName.getText
        case SFunExpr(_, fun) => fun.getName.getText
        case SFunApp(_, call, args) => name(call)
        case SNew(_, lhs) => name(lhs)
        case SVarRef(_, id) => id.getText
        case SDot(_, obj, member) => member.getText
        case SBracket(_, lhs, expr) => name(expr)
        case SStringLiteral(_, qs, es) => es
        case _ => ""
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

  def walkAST[A](f: (Any, Any, A) => A, g: ((Any, Any) => A => A) = (_: Any,_: Any) => (b: A) => b)(parent: Any, node: Any)(a: A): A = {
    val rec = walkAST(f,g)(_,_)
    node match {
      case SProgram(info, body) =>
        f(parent, node, a) >>
          rec(node, body) >>
          g(parent, node)
      case SNoOp(info, desc) =>
        f(parent, node, a) >>
          g(parent, node)
      case SStmtUnit(info, stmts) =>
        f(parent, node, a) >>
          rec(node, stmts) >>
          g(parent, node)
      case SFunDecl(info, ftn, strict) =>
        f(parent, node, a) >>
          rec(node, ftn) >>
          g(parent, node)
      case SBlock(info, stmts, internal) =>
        f(parent, node, a) >>
          rec(node, stmts) >>
          g(parent, node)
      case SVarStmt(info, vds) =>
        f(parent, node, a) >>
          rec(node, vds) >>
          g(parent, node)
      case SEmptyStmt(info) =>
        f(parent, node, a) >>
          g(parent, node)
      case SExprStmt(info, expr, internal) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          g(parent, node)
      case SIf(info, cond, trueBranch, falseBranch) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, trueBranch) >>
          rec(node, falseBranch) >>
          g(parent, node)
      case SDoWhile(info, body, cond) =>
        f(parent, node, a) >>
          rec(node, body) >>
          rec(node, cond) >>
          g(parent, node)
      case SWhile(info, cond, body) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, body) >>
          g(parent, node)
      case SFor(info, init, cond, action, body) =>
        f(parent, node, a) >>
          rec(node, init) >>
          rec(node, cond) >>
          rec(node, action) >>
          rec(node, body) >>
          g(parent, node)
      case SForIn(info, lhs, expr, body) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, expr) >>
          rec(node, body) >>
          g(parent, node)
      case SForVar(info, vars, cond, action, body) =>
        f(parent, node, a) >>
          rec(node, vars) >>
          rec(node, cond) >>
          rec(node, action) >>
          rec(node, body) >>
          g(parent, node)
      case SForVarIn(info, _var, expr, body) =>
        f(parent, node, a) >>
          rec(node, _var) >>
          rec(node, expr) >>
          rec(node, body) >>
          g(parent, node)
      case SContinue(info, target) =>
        f(parent, node, a) >>
          rec(node, target) >>
          g(parent, node)
      case SBreak(info, target) =>
        f(parent, node, a) >>
          rec(node, target) >>
          g(parent, node)
      case SReturn(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          g(parent, node)
      case SWith(info, expr, stmt) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          rec(node, stmt) >>
          g(parent, node)
      case SSwitch(info, cond, frontCases, _def, backCases) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, frontCases) >>
          rec(node, _def) >>
          rec(node, backCases) >>
          g(parent, node)
      case SLabelStmt(info, label, stmt) =>
        f(parent, node, a) >>
          rec(node, label) >>
          rec(node, stmt) >>
          g(parent, node)
      case SThrow(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          g(parent, node)
      case STry(info, body, catchBlock, fin) =>
        f(parent, node, a) >>
          rec(node, body) >>
          rec(node, catchBlock) >>
          rec(node, fin) >>
          g(parent, node)
      case SDebugger(info) =>
        f(parent, node, a) >>
          g(parent, node)
      case SSourceElements(info, body, strict) =>
        f(parent, node, a) >>
          rec(node, body) >>
          g(parent, node)
      case SVarDecl(info, id, expr, strict) =>
        f(parent, node, a) >>
          rec(node, id) >>
          rec(node, expr) >>
          g(parent, node)
      case SCase(info, cond, body) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, body) >>
          g(parent, node)
      case SCatch(info, id, body) =>
        f(parent, node, a) >>
          rec(node, id) >>
          rec(node, body) >>
          g(parent, node)
      case SExprList(info, exprs) =>
        f(parent, node, a) >>
          rec(node, exprs) >>
          g(parent, node)
      case SCond(info, cond, trueBranch, falseBranch) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, trueBranch) >>
          rec(node, falseBranch) >>
          g(parent, node)
      case SInfixOpApp(info, left, op, right) =>
        f(parent, node, a) >>
          rec(node, left) >>
          rec(node, op) >>
          rec(node, right) >>
          g(parent, node)
      case SPrefixOpApp(info, op, right) =>
        f(parent, node, a) >>
          rec(node, op) >>
          rec(node, right) >>
          g(parent, node)
      case SUnaryAssignOpApp(info, lhs, op) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, op) >>
          g(parent, node)
      case SAssignOpApp(info, lhs, op, right) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, op) >>
          rec(node, right) >>
          g(parent, node)
      case SThis(info) =>
        f(parent, node, a) >>
          g(parent, node)
      case SNull(info) =>
        f(parent, node, a) >>
          g(parent, node)
      case SBool(info, bool) =>
        f(parent, node, a) >>
          g(parent, node)
      case SDoubleLiteral(info, text, num) =>
        f(parent, node, a) >>
          g(parent, node)
      case SIntLiteral(info, intVal, radix) =>
        f(parent, node, a) >>
          g(parent, node)
      case SStringLiteral(info, quote, escaped) =>
        f(parent, node, a) >>
          g(parent, node)
      case SRegularExpression(info, body, flag) =>
        f(parent, node, a) >>
          g(parent, node)
      case SVarRef(info, id) =>
        f(parent, node, a) >>
          rec(node, id) >>
          g(parent, node)
      case SArrayExpr(info, elements) =>
        f(parent, node, a) >>
          rec(node, elements) >>
          g(parent, node)
      case SArrayNumberExpr(info, elements) =>
        f(parent, node, a) >>
          rec(node, elements) >>
          g(parent, node)
      case SObjectExpr(info, members) =>
        f(parent, node, a) >>
          rec(node, members) >>
          g(parent, node)
      case SParenthesized(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          g(parent, node)
      case SFunExpr(info, ftn) =>
        f(parent, node, a) >>
          rec(node, ftn) >>
          g(parent, node)
      case SBracket(info, obj, index) =>
        f(parent, node, a) >>
          rec(node, obj) >>
          rec(node, index) >>
          g(parent, node)
      case SDot(info, obj, member) =>
        f(parent, node, a) >>
          rec(node, obj) >>
          rec(node, member) >>
          g(parent, node)
      case SNew(info, lhs) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          g(parent, node)
      case SFunApp(info, fun, args) =>
        f(parent, node, a) >>
          rec(node, fun) >>
          rec(node, args) >>
          g(parent, node)
      case SPropId(info, id) =>
        f(parent, node, a) >>
          rec(node, id) >>
          g(parent, node)
      case SPropStr(info, str) =>
        f(parent, node, a) >>
          g(parent, node)
      case SPropNum(info, num) =>
        f(parent, node, a) >>
          g(parent, node)
      case SField(info, prop, expr) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, expr) >>
          g(parent, node)
      case SGetProp(info, prop, ftn) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, ftn) >>
          g(parent, node)
      case SSetProp(info, prop, ftn) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, ftn) >>
          g(parent, node)
      case SId(info, text, uniqueName, _with) =>
        f(parent, node, a) >>
          g(parent, node)
      case SOp(info, text) =>
        f(parent, node, a) >>
          g(parent, node)
      case SLabel(info, id) =>
        f(parent, node, a) >>
          rec(node, id) >>
          g(parent, node)
      case SComment(info, comment) =>
        f(parent, node, a) >>
          g(parent, node)
      case STopLevel(fds, vds, stmts) =>
        f(parent, node, a) >>
          rec(node, fds) >>
          rec(node, vds) >>
          rec(node, stmts) >>
          g(parent, node)
      case SFunctional(fds, vds, stmts, id, params) =>
        f(parent, node, a) >>
          rec(node, fds) >>
          rec(node, vds) >>
          rec(node, stmts) >>
          rec(node, id) >>
          rec(node, params) >>
          g(parent, node)
      case list: List[_] =>
        list.foldLeft(a)((b, node) => rec(parent, node)(b))
      case Some(n) =>
        f(parent, node, a) >>
          rec(parent, n) >>
          g(parent, node)
      case None =>
        f(parent, node, a) >>
          g(parent, node)
    }
  }
}
