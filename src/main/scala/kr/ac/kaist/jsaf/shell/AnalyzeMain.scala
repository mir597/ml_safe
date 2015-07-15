/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import kr.ac.kaist.jsaf.nodes.{ASTSpanInfo, Program}
import kr.ac.kaist.jsaf.nodes_util.{Span, JSAstToConcrete}
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MHashMap}
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.Shell
import scala.io.Source
import net.liftweb.json._

////////////////////////////////////////////////////////////////////////////////
// Analyze
////////////////////////////////////////////////////////////////////////////////
object AnalyzeMain {

  def parseFromFile(decls: List[Any], calls: List[Any], filename: String): MHashMap[(Any, Any), Int] = {
    val map = MHashMap[(Any, Any), Int]()

    // Initial result value is 0.
    decls.foreach(d => {
      calls.foreach(c => {
        map += (d, c) -> 0
      })
    })

    def span(i: Span): (String, Int, Int, Int) = {
      val begin = i.getBegin
      val end = i.getEnd
      (begin.getFileNameOnly, begin.getLine,begin.getOffset, end.getOffset)
    }

    def parse_span(s: String, decl: Boolean): (String, Int, Int, Int) = {
      val a1 = s.split("@")
      val filename = a1(0)
      val others = a1(1)
      val a2 = others.split(Array(':', '-'))
      val line = a2(0)
      val start_offset = a2(1)
      val end_offset = a2(2)
      val p = if (decl) 0 else 1
      (filename, line.toInt, start_offset.toInt, end_offset.toInt - p)
    }

    def findmap[A](m: HashMap[A,Any])(s: A): Any = {
      try
        m(s)
      catch {
        case _: Throwable =>
          System.out.println("* Error: cannot find a case for "+s)
          System.out.println("* Dump for hash map")
          m.foreach(kv => {
            System.out.println(kv._1+" -> "+kv._2)
          })
          throw new InternalError()
      }
    }

    val spanmap =
      (calls ++ decls).foldLeft(HashMap[(String,Int,Int,Int),Any]())((m, n) => {
        n match {
          case SFunDecl(info, _, _) => m + (span(info.getSpan) -> n)
          case SFunExpr(info, _) => m + (span(info.getSpan) -> n)
          case SFunApp(info, _, _) => m + (span(info.getSpan) -> n)
        }
      })

    val find = findmap(spanmap)(_)

    // The file must contain all the function call histories for a given executions.
    // A function call consists of 8 integers which format is as follows:
    //  start_line_of_decl:start_column:end_line:end_column:start_line_of_callexpr:start_column:end_line_end_column
    val json = parse(Source.fromFile(filename) mkString)

    json.children.foreach {
      case JField(name, value) =>
        value match {
          case JArray(list) =>
            list.foreach {
              case JString(str) =>
                if (str.contains("@")) {
                  // user function call
                  val declsite = parse_span(str, false)
                  val callsite = parse_span(name, true)

                  val dn = find(declsite)
                  val cn = find(callsite)

                  map += (dn, cn) -> 1
                } else {
                  // TODO built-in function calls should be considered.
                  System.err.println(name + " -> "+str)
                }
              case _ =>
            }

          case _ =>
        }
      case _ =>
    }

    map
  }

  def analyze: Int = {
    if (Shell.params.FileNames.isEmpty) throw new UserError("Need a file to analyze")
    val fileNames = JavaConversions.seqAsJavaList(Shell.params.FileNames)

    // Initialize
    val return_code = 0
    System.err.println("\n* Initialize *")

    // Read a JavaScript file and translate to IR
    val start = System.nanoTime
    val program: Program = Parser.fileToAST(fileNames)

    val parseTime = (System.nanoTime - start) / 1000000000.0
    System.err.println("# Time for parsing(s): %.2f\n".format(parseTime))

    val hoistedProgram = new Hoister(program).doit().asInstanceOf[Program]
    val disambiguatedProgram = new Disambiguator(hoistedProgram, disambiguateOnly = false).doit().asInstanceOf[Program]

    val (decls, calls) = walkAST(null, disambiguatedProgram)(Nil, Nil)

    System.err.println("** Decls **")
    decls.foreach {
      case SFunDecl(info, ftn, strict) =>
        System.err.println("- "+info.getSpan.toString+": "+ftn.getName.getUniqueName)
      case SFunExpr(info, ftn) =>
        System.err.println("- "+info.getSpan.toString+": "+ftn.getName.getUniqueName)
    }

    System.err.println("** Calls **")
    calls.foreach {
      case s@SFunApp(info, fun, args) =>
        val str = JSAstToConcrete.walk(s)
        System.err.println("- " + info.getSpan.toString + ": " + str)
    }

    val feature_map: MHashMap[(Any, Any), List[Int]] = new MHashMap()

    // Parse the result.
    val result_map = parseFromFile(decls, calls, Shell.params.opt_ResultFileName)

    // Initialize features.
    decls.foreach(decl => {
      calls.foreach(call => {
        // initial feature vectors for each case: 0 0 0
        feature_map += (decl,call) -> (0::0::0::Nil)
      })
    })

    System.err.println("* data")
    decls.foreach(decl => {
      calls.foreach(call => {
        val bitvectors = feature_map((decl, call))
        bitvectors.foreach(v => System.out.print(v+" "))
        System.out.print(":")
        val answer = result_map((decl, call))
        System.out.println(answer)
      })
    })

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
