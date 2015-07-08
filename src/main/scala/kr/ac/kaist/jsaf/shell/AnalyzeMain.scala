/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.nodes_util.JSAstToConcrete
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.Shell

////////////////////////////////////////////////////////////////////////////////
// Analyze
////////////////////////////////////////////////////////////////////////////////
object AnalyzeMain {

  def analyze: Int = {

    if (Shell.params.FileNames.isEmpty) throw new UserError("Need a file to analyze")
    val fileNames = JavaConversions.seqAsJavaList(Shell.params.FileNames)

    // Initialize
    val return_code = 0
    System.out.println("\n* Initialize *")

    // Read a JavaScript file and translate to IR
    val start = System.nanoTime
    val program: Program = Parser.fileToAST(fileNames)

    val parseTime = (System.nanoTime - start) / 1000000000.0
    printf("# Time for parsing(s): %.2f\n", parseTime)

    val hoistedProgram = new Hoister(program).doit().asInstanceOf[Program]
    val disambiguatedProgram = new Disambiguator(hoistedProgram, disambiguateOnly = false).doit().asInstanceOf[Program]

    val (decls, calls) = walkAST(null, disambiguatedProgram)(Nil, Nil)

    System.out.println("** Decls **")
    decls.foreach {
      case SFunDecl(info, ftn, strict) =>
        System.out.println("- "+info.getSpan.toString+": "+ftn.getName.getUniqueName)
      case SFunExpr(info, ftn) =>
        System.out.println("- "+info.getSpan.toString+": "+ftn.getName.getUniqueName)
    }

    System.out.println("** Calls **")
    calls.foreach {
      case s@SFunApp(info, fun, args) =>
        val str = JSAstToConcrete.walk(s)
        System.out.println("- " + info.getSpan.toString + ": " + str)
    }

    return_code
  }

  implicit def any2waterfall[A](a: A): Object {def >>[B](f: (A) => B): B} = new AnyRef{
    def >>[B](f: A=>B) = f(a)
  }

  def walkAST(parent: Any, node: Any)(pair: (List[Any], List[Any])): (List[Any], List[Any]) = node match {
    case SProgram(info, body) =>
      walkAST(node, body)(pair)
    case SNoOp(info, desc) =>
      pair
    case SStmtUnit(info, stmts) =>
      walkAST(node, stmts)(pair)
    case SFunDecl(info, ftn, strict) =>
      val new_pair = (node::pair._1, pair._2)
      walkAST(node, ftn)(new_pair)
    case SBlock(info, stmts, internal) =>
      walkAST(node, stmts)(pair)
    case SVarStmt(info, vds) =>
      walkAST(node, vds)(pair)
    case SEmptyStmt(info) =>
      pair
    case SExprStmt(info, expr, internal) =>
      walkAST(node, expr)(pair)
    case SIf(info, cond, trueBranch, falseBranch) =>
      walkAST(node, cond)(pair) >>
        walkAST(node, trueBranch) >>
        walkAST(node, falseBranch)
    case SDoWhile(info, body, cond) =>
      walkAST(node, body)(pair) >>
        walkAST(node, cond)
    case SWhile(info, cond, body) =>
      walkAST(node, cond)(pair) >>
        walkAST(node, body)
    case SFor(info, init, cond, action, body) =>
      walkAST(node, init)(pair) >>
        walkAST(node, cond) >>
        walkAST(node, action) >>
        walkAST(node, body)
    case SForIn(info, lhs, expr, body) =>
      walkAST(node, lhs)(pair) >>
        walkAST(node, expr) >>
        walkAST(node, body)
    case SForVar(info, vars, cond, action, body) =>
      walkAST(node, vars)(pair) >>
        walkAST(node, cond) >>
        walkAST(node, action) >>
        walkAST(node, body)
    case SForVarIn(info, _var, expr, body) =>
      walkAST(node, _var)(pair) >>
        walkAST(node, expr) >>
        walkAST(node, body)
    case SContinue(info, target) =>
      walkAST(node, target)(pair)
    case SBreak(info, target) =>
      walkAST(node, target)(pair)
    case SReturn(info, expr) =>
      walkAST(node, expr)(pair)
    case SWith(info, expr, stmt) =>
      walkAST(node, expr)(pair) >>
        walkAST(node, stmt)
    case SSwitch(info, cond, frontCases, _def, backCases) =>
      walkAST(node, cond)(pair) >>
        walkAST(node, frontCases) >>
        walkAST(node, _def) >>
        walkAST(node, backCases)
    case SLabelStmt(info, label, stmt) =>
      walkAST(node, label)(pair) >>
        walkAST(node, stmt)
    case SThrow(info, expr) =>
      walkAST(node, expr)(pair)
    case STry(info, body, catchBlock, fin) =>
      walkAST(node, body)(pair) >>
        walkAST(node, catchBlock) >>
        walkAST(node, fin)
    case SDebugger(info) =>
      pair
    case SSourceElements(info, body, strict) =>
      walkAST(node, body)(pair)
    case SVarDecl(info, id, expr, strict) =>
      walkAST(node, id)(pair) >>
        walkAST(node, expr)
    case SCase(info, cond, body) =>
      walkAST(node, cond)(pair) >>
        walkAST(node, body)
    case SCatch(info, id, body) =>
      walkAST(node, id)(pair) >>
        walkAST(node, body)
    case SExprList(info, exprs) =>
      walkAST(node, exprs)(pair)
    case SCond(info, cond, trueBranch, falseBranch) =>
      walkAST(node, cond)(pair) >>
        walkAST(node, trueBranch) >>
        walkAST(node, falseBranch)
    case SInfixOpApp(info, left, op, right) =>
      walkAST(node, left)(pair) >>
        walkAST(node, op) >>
        walkAST(node, right)
    case SPrefixOpApp(info, op, right) =>
      walkAST(node, op)(pair) >>
        walkAST(node, right)
    case SUnaryAssignOpApp(info, lhs, op) =>
      walkAST(node, lhs)(pair) >>
        walkAST(node, op)
    case SAssignOpApp(info, lhs, op, right) =>
      walkAST(node, lhs)(pair) >>
        walkAST(node, op) >>
        walkAST(node, right)
    case SThis(info) =>
      pair
    case SNull(info) =>
      pair
    case SBool(info, bool) =>
      pair
    case SDoubleLiteral(info, text, num) =>
      pair
    case SIntLiteral(info, intVal, radix) =>
      pair
    case SStringLiteral(info, quote, escaped) =>
      pair
    case SRegularExpression(info, body, flag) =>
      pair
    case SVarRef(info, id) =>
      walkAST(node, id)(pair)
    case SArrayExpr(info, elements) =>
      walkAST(node, elements)(pair)
    case SArrayNumberExpr(info, elements) =>
      walkAST(node, elements)(pair)
    case SObjectExpr(info, members) =>
      walkAST(node, members)(pair)
    case SParenthesized(info, expr) =>
      walkAST(node, expr)(pair)
    case SFunExpr(info, ftn) =>
      val new_pair = (node::pair._1, pair._2)
      walkAST(node, ftn)(new_pair)
    case SBracket(info, obj, index) =>
      walkAST(node, obj)(pair) >>
        walkAST(node, index)
    case SDot(info, obj, member) =>
      walkAST(node, obj)(pair) >>
        walkAST(node, member)
    case SNew(info, lhs) =>
      walkAST(node, lhs)(pair)
    case SFunApp(info, fun, args) =>
      val new_pair = (pair._1, node::pair._2)
      walkAST(node, fun)(new_pair) >>
        walkAST(node, args)
    case SPropId(info, id) =>
      walkAST(node, id)(pair)
    case SPropStr(info, str) =>
      pair
    case SPropNum(info, num) =>
      pair
    case SField(info, prop, expr) =>
      walkAST(node, prop)(pair) >>
        walkAST(node, expr)
    case SGetProp(info, prop, ftn) =>
      walkAST(node, prop)(pair) >>
        walkAST(node, ftn)
    case SSetProp(info, prop, ftn) =>
      walkAST(node, prop)(pair) >>
        walkAST(node, ftn)
    case SId(info, text, uniqueName, _with) =>
      pair
    case SOp(info, text) =>
      pair
    case SLabel(info, id) =>
      walkAST(node, id)(pair)
    case SComment(info, comment) =>
      pair
    case STopLevel(fds, vds, stmts) =>
      walkAST(node, fds)(pair) >>
        walkAST(node, vds) >>
        walkAST(node, stmts)
    case SFunctional(fds, vds, stmts, id, params) =>
      walkAST(node, fds)(pair) >>
        walkAST(node, vds) >>
        walkAST(node, stmts) >>
        walkAST(node, id) >>
        walkAST(node, params)
    case list: List[_] =>
      list.foldLeft(pair)((p, node) => walkAST(parent, node)(p))
    case Some(n) =>
      walkAST(parent, n)(pair)
    case None =>
      pair
  }
}
