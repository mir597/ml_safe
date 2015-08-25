package kr.ac.kaist.jsaf.syntactic

import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.{HashMap, HashSet}
import kr.ac.kaist.jsaf.string

/**
 * Created by ysko on 15. 8. 3.
 */
object OneshotCallMatcher extends PreAnalysis {
  override def featureName: String = "One-shot call"
  type t = Callgraph

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

  def process(decllist: List[Any], calllist: List[Any], cg: Callgraph): (List[Any], List[Any], Callgraph) = {
    def process_(declset: HashSet[Any], callset: HashSet[Any], calls: List[Any], cg: t): (HashSet[Any], HashSet[Any], t) = {
      calls match {
        case h::t =>
          val set =
            h match {
              case SFunApp(_, fun, _) => decls(fun)
              case SNew(_, lhs) => decls(lhs)
            }
          val (ndeclset, ncallset, ncg) =
            if (set.nonEmpty) (declset ++ set, callset + h, cg + (h -> set))
            else (declset, callset, cg)
          process_(ndeclset, ncallset, t, ncg)
        case Nil => (declset, callset, cg)
      }
    }

    val (usedDecls, usedCalls, ncg) = process_(HashSet[Any](), HashSet[Any](), calllist, cg)

    val ncalls = calllist.filter(!usedCalls.contains(_))
    (decllist, calllist, ncg)
//    (decllist, ncalls, ncg)
  }
}
