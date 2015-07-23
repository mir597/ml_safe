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

  def walkAST[A](f: (Any, Any, A) => A)(parent: Any, node: Any)(a: A): A = {
    val rec = walkAST(f)(_,_)
    node match {
      case SProgram(info, body) =>
        f(parent, node, a) >>
          rec(node, body)
      case SNoOp(info, desc) =>
        f(parent, node, a)
      case SStmtUnit(info, stmts) =>
        f(parent, node, a) >>
          rec(node, stmts)
      case SFunDecl(info, ftn, strict) =>
        f(parent, node, a) >>
          rec(node, ftn)
      case SBlock(info, stmts, internal) =>
        f(parent, node, a) >>
          rec(node, stmts)
      case SVarStmt(info, vds) =>
        f(parent, node, a) >>
          rec(node, vds)
      case SEmptyStmt(info) =>
        f(parent, node, a)
      case SExprStmt(info, expr, internal) =>
        f(parent, node, a) >>
          rec(node, expr)
      case SIf(info, cond, trueBranch, falseBranch) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, trueBranch) >>
          rec(node, falseBranch)
      case SDoWhile(info, body, cond) =>
        f(parent, node, a) >>
          rec(node, body) >>
          rec(node, cond)
      case SWhile(info, cond, body) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, body)
      case SFor(info, init, cond, action, body) =>
        f(parent, node, a) >>
          rec(node, init) >>
          rec(node, cond) >>
          rec(node, action) >>
          rec(node, body)
      case SForIn(info, lhs, expr, body) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, expr) >>
          rec(node, body)
      case SForVar(info, vars, cond, action, body) =>
        f(parent, node, a) >>
          rec(node, vars) >>
          rec(node, cond) >>
          rec(node, action) >>
          rec(node, body)
      case SForVarIn(info, _var, expr, body) =>
        f(parent, node, a) >>
          rec(node, _var) >>
          rec(node, expr) >>
          rec(node, body)
      case SContinue(info, target) =>
        f(parent, node, a) >>
          rec(node, target)
      case SBreak(info, target) =>
        f(parent, node, a) >>
          rec(node, target)
      case SReturn(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr)
      case SWith(info, expr, stmt) =>
        f(parent, node, a) >>
          rec(node, expr) >>
          rec(node, stmt)
      case SSwitch(info, cond, frontCases, _def, backCases) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, frontCases) >>
          rec(node, _def) >>
          rec(node, backCases)
      case SLabelStmt(info, label, stmt) =>
        f(parent, node, a) >>
          rec(node, label) >>
          rec(node, stmt)
      case SThrow(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr)
      case STry(info, body, catchBlock, fin) =>
        f(parent, node, a) >>
          rec(node, body) >>
          rec(node, catchBlock) >>
          rec(node, fin)
      case SDebugger(info) =>
        f(parent, node, a)
      case SSourceElements(info, body, strict) =>
        f(parent, node, a) >>
          rec(node, body)
      case SVarDecl(info, id, expr, strict) =>
        f(parent, node, a) >>
          rec(node, id) >>
          rec(node, expr)
      case SCase(info, cond, body) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, body)
      case SCatch(info, id, body) =>
        f(parent, node, a) >>
          rec(node, id) >>
          rec(node, body)
      case SExprList(info, exprs) =>
        f(parent, node, a) >>
          rec(node, exprs)
      case SCond(info, cond, trueBranch, falseBranch) =>
        f(parent, node, a) >>
          rec(node, cond) >>
          rec(node, trueBranch) >>
          rec(node, falseBranch)
      case SInfixOpApp(info, left, op, right) =>
        f(parent, node, a) >>
          rec(node, left) >>
          rec(node, op) >>
          rec(node, right)
      case SPrefixOpApp(info, op, right) =>
        f(parent, node, a) >>
          rec(node, op) >>
          rec(node, right)
      case SUnaryAssignOpApp(info, lhs, op) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, op)
      case SAssignOpApp(info, lhs, op, right) =>
        f(parent, node, a) >>
          rec(node, lhs) >>
          rec(node, op) >>
          rec(node, right)
      case SThis(info) =>
        f(parent, node, a)
      case SNull(info) =>
        f(parent, node, a)
      case SBool(info, bool) =>
        f(parent, node, a)
      case SDoubleLiteral(info, text, num) =>
        f(parent, node, a)
      case SIntLiteral(info, intVal, radix) =>
        f(parent, node, a)
      case SStringLiteral(info, quote, escaped) =>
        f(parent, node, a)
      case SRegularExpression(info, body, flag) =>
        f(parent, node, a)
      case SVarRef(info, id) =>
        f(parent, node, a) >>
          rec(node, id)
      case SArrayExpr(info, elements) =>
        f(parent, node, a) >>
          rec(node, elements)
      case SArrayNumberExpr(info, elements) =>
        f(parent, node, a) >>
          rec(node, elements)
      case SObjectExpr(info, members) =>
        f(parent, node, a) >>
          rec(node, members)
      case SParenthesized(info, expr) =>
        f(parent, node, a) >>
          rec(node, expr)
      case SFunExpr(info, ftn) =>
        f(parent, node, a) >>
          rec(node, ftn)
      case SBracket(info, obj, index) =>
        f(parent, node, a) >>
          rec(node, obj) >>
          rec(node, index)
      case SDot(info, obj, member) =>
        f(parent, node, a) >>
          rec(node, obj) >>
          rec(node, member)
      case SNew(info, lhs) =>
        f(parent, node, a) >>
          rec(node, lhs)
      case SFunApp(info, fun, args) =>
        f(parent, node, a) >>
          rec(node, fun) >>
          rec(node, args)
      case SPropId(info, id) =>
        f(parent, node, a) >>
          rec(node, id)
      case SPropStr(info, str) =>
        f(parent, node, a)
      case SPropNum(info, num) =>
        f(parent, node, a)
      case SField(info, prop, expr) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, expr)
      case SGetProp(info, prop, ftn) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, ftn)
      case SSetProp(info, prop, ftn) =>
        f(parent, node, a) >>
          rec(node, prop) >>
          rec(node, ftn)
      case SId(info, text, uniqueName, _with) =>
        f(parent, node, a)
      case SOp(info, text) =>
        f(parent, node, a)
      case SLabel(info, id) =>
        f(parent, node, a) >>
          rec(node, id)
      case SComment(info, comment) =>
        f(parent, node, a)
      case STopLevel(fds, vds, stmts) =>
        f(parent, node, a) >>
          rec(node, fds) >>
          rec(node, vds) >>
          rec(node, stmts)
      case SFunctional(fds, vds, stmts, id, params) =>
        f(parent, node, a) >>
          rec(node, fds) >>
          rec(node, vds) >>
          rec(node, stmts) >>
          rec(node, id) >>
          rec(node, params)
      case list: List[_] =>
        list.foldLeft(a)((b, node) => rec(parent, node)(b))
      case Some(n) =>
        f(parent, node, a) >>
          rec(parent, n)
      case None =>
        f(parent, node, a)
    }
  }
}
