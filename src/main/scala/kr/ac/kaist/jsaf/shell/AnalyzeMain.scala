/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import java.util

import kr.ac.kaist.jsaf.features.SimpleName
import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.nodes_util.{Span, JSAstToConcrete}
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.immutable.{HashSet, HashMap}
import scala.collection.mutable.{HashMap => MHashMap}
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.Shell
import scala.io.Source
import net.liftweb.json._
import kr.ac.kaist.jsaf._
import kr.ac.kaist.jsaf.features._

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

    def toString(n: Any) = n match {
      case SFunDecl(info, ftn, strict) =>
        info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles+": "+ftn.getName.getText
      case SFunExpr(info, ftn) =>
        info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles+": "+ftn.getName.getText
      case s@SFunApp(info, fun, args) =>
        val str = JSAstToConcrete.walk(fun)
        info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles + ": " + str
      case s@SNew(info, lhs) =>
        val str = JSAstToConcrete.walk(lhs)
        info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles + ": " + str
    }

    System.err.println("** Decls **")
    decls.foreach (n => System.err.println("- "+toString(n)))

    System.err.println("** Calls **")
    calls.foreach (n => System.err.println("- "+toString(n)))

    val init_map: HashMap[(Any, Any), List[Int]] = HashMap()

    def init_set(map: HashMap[(Any, Any), List[Int]]) = {
      decls.foldLeft(map)((map_1, decl) => {
        calls.foldLeft(map_1)((map_2, call) => {
          // initial feature vectors with an empty list
          map_2 + ((decl, call) -> Nil)
        })
      })
    }

    // Parse the result.
    val result_map = parseFromFile(decls, calls, Shell.params.opt_ResultFileName)

    // Initialize features.
    val feature_map =
      init_set(init_map) >>
        Classifier.genFeature >>
        SimpleName.genFeature >>
        PropName.genFeature(PropName.init(disambiguatedProgram))

    System.err.println("* data")
    calls.foreach(call => {
      decls.foreach(decl => {
        val bitvectors = feature_map((decl, call))
        System.err.print(toString(call) + " => " +toString(decl)+ "     ")
        bitvectors.foreach(v => {
          System.out.print(v + " ")
        })
        System.out.print(":")
        val answer = result_map((decl, call))
        System.out.println(answer)
      })
    })

    return_code
  }
}
