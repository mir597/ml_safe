/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import java.util

import kr.ac.kaist.jsaf.nodes.{VarRef, Id, Program}
import kr.ac.kaist.jsaf.nodes_util.{Span, JSAstToConcrete}
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.JavaConverters._
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

    def findmap(m: HashMap[(String, Int, Int, Int),Any])(s: (String, Int, Int, Int)): Option[Any] = {
      m.get(s) match {
        case Some(v) => Some(v)
        case None =>
          val s2 = (s._1, s._2, s._3, s._4 - 1)
          m.get(s2) match {
            case Some(v) => Some(v)
            case None =>
              val s3 = (s._1, s._2, s._3, s._4 + 1)
              m.get(s3)
          }
      }
    }

    val spanmap =
      (calls ++ decls).foldLeft(HashMap[(String,Int,Int,Int),Any]())((m, n) => {
        n match {
          case SFunDecl(info, _, _) => m + (span(info.getSpan) -> n)
          case SFunExpr(info, _) => m + (span(info.getSpan) -> n)
          case SFunApp(info, _, _) => m + (span(info.getSpan) -> n)
          case SNew(info, _) => m + (span(info.getSpan) -> n)
        }
      })

    val find = findmap(spanmap)(_)

    // The file must contain all the function call histories for a given executions.
    // A function call consists of 8 integers which format is as follows:
    //  start_line_of_decl:start_column:end_line:end_column:start_line_of_callexpr:start_column:end_line_end_column
    if (filename != null) {
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

                    (dn, cn) match {
                      case (Some(d), Some(c)) => map += (d, c) -> 1
                      case _ =>
                      // offset mismatch.
                    }
                  } else {
                    // TODO built-in function calls should be considered.
                    System.err.println(name + " -> " + str)
                  }
                case _ =>
              }

            case _ =>
          }
        case _ =>
      }
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

    // Function Decl/Expr and Callsite Collector
    def collectDeclCallPair(parent: Any, node: Any, pair: (List[Any], List[Any])) = node match {
      case SFunDecl(info, ftn, strict) =>
        (node::pair._1, pair._2)
      case SFunExpr(info, ftn) =>
        (node::pair._1, pair._2)
      case SNew(info, lhs) =>
        lhs match {
          case SFunApp(_, _, _) => pair
          case _ => (pair._1, node::pair._2) // case for 'new A'
        }
      case SFunApp(info, fun, args) =>
        (pair._1, node::pair._2)
      case _ => pair
    }

    val (decls, calls) = walkAST(collectDeclCallPair)(null, disambiguatedProgram)(Nil, Nil)

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

      case s@SNew(info, lhs) =>
        val str = JSAstToConcrete.walk(s)
        System.err.println("- " + info.getSpan.toString + ": " + str)
    }

    val init_map: HashMap[(Any, Any), List[Int]] = HashMap()

    def init_set(map: HashMap[(Any, Any), List[Int]]) = {
      decls.foldLeft(map)((map_1, decl) => {
        calls.foldLeft(map_1)((map_2, call) => {
          // initial feature vectors with an empty list
          map_2 + ((decl, call) -> Nil)
        })
      })
    }

    def exprClassFeature(map: HashMap[(Any, Any), List[Int]]) = {
      // classify the type of call expressions.
      // 1: simple function call
      // 2: method call
      map.map(f => {
        val dc = f._1
        val vectors = f._2
        val callexpr = dc._2
        val vec =
          callexpr match {
            case SFunApp(info, fun, args) =>
              fun match {
                case _: VarRef => 1
                case _ => 2
              }
            case SNew(_, lhs) =>
              lhs match {
                case _: VarRef => 1
                case _ => 2
              }
          }

        (dc, vec::vectors)
      })
    }

    def name(n: Any): String = {
      n match {
        case SFunDecl(_, fun, _) =>
          fun.getName.getText
        case SFunExpr(_, fun) =>
          fun.getName.getText
        case SFunApp(_, call, args) =>
          name(call)
        case SNew(_, lhs) =>
          name(lhs)
        case SVarRef(_, id) =>
          id.getText
        case SDot(_, obj, member) =>
          member.getText
        case _ => ""
      }
    }

    def nameFeature(map: HashMap[(Any, Any), List[Int]]) = {
      map.map(f => {
        val dc = f._1
        val vectors = f._2
        val callname = name(dc._2)
        val declname = name(dc._1)

        val vec =
          if (callname.equals(declname)) 1
          else 0

        (dc, vec::vectors)
      })
    }

    // Parse the result.
    val result_map = parseFromFile(decls, calls, Shell.params.opt_ResultFileName)

    // Initialize features.
    val feature_map =
      init_set(init_map) >>
        exprClassFeature >>
        nameFeature

    System.err.println("* data")
    decls.foreach(decl => {
      calls.foreach(call => {
        val bitvectors = feature_map((decl, call))
        bitvectors.foreach(v => System.out.print(v + " "))
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
