/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import java.io.{PrintWriter, File}

import kr.ac.kaist.jsaf.features.SimpleName
import kr.ac.kaist.jsaf.ml.CallHistoryParser
import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.immutable.HashMap
import scala.collection.mutable.{ HashMap => MHashMap }
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf._
import kr.ac.kaist.jsaf.features._

////////////////////////////////////////////////////////////////////////////////
// Analyze
////////////////////////////////////////////////////////////////////////////////
object AnalyzeMain {

  def eprintln(s: String) = System.err.println(s)
  def eprint(s: String) = System.err.print(s)

  def analyze: Int = {
    if (Shell.params.FileNames.isEmpty) throw new UserError("Need a file to analyze")
    val fileNames = JavaConversions.seqAsJavaList(Shell.params.FileNames)

    // Initialize
    val return_code = 0
    eprintln("\n* Initialize *")

    // Read a JavaScript file and translate to IR
    val start = System.nanoTime
    val program: Program = Parser.fileToAST(fileNames)

    val parseTime = (System.nanoTime - start) / 1000000000.0
    eprintln("# Time for parsing(s): %.2f\n".format(parseTime))

    val poststart = System.nanoTime
    val hoistedProgram = new Hoister(program).doit().asInstanceOf[Program]
    val disambiguatedProgram = new Disambiguator(hoistedProgram, disambiguateOnly = false).doit().asInstanceOf[Program]
    val postTime = (System.nanoTime - poststart) / 1000000000.0
    eprintln("# Time for hoisting and disambiguation(s): %.2f\n".format(postTime))

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

    val initstart = System.nanoTime
    val (decls, calls) = walkAST(collectDeclCallPair)(null, disambiguatedProgram)(Nil, Nil)
    val initTime = (System.nanoTime - initstart) / 1000000000.0
    eprintln("# Time for extracting function decl and call exprs(s): %.2f\n".format(initTime))

    if (Shell.params.opt_debug) {
//      eprintln("** Decls **")
//      decls.foreach (n => eprintln("- "+string(n)))

//      eprintln("** Calls **")
//      calls.foreach (n => eprintln("- "+string(n)))
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

    // Parse the result.
    val inputstart = System.nanoTime
    val result_map = CallHistoryParser.parseFromFile(decls, calls, Shell.params.opt_ResultFileName)
    val wala_map: MHashMap[(Any, Any), Int] =
      if (Shell.params.opt_WALAFileName != null) CallHistoryParser.parseFromFile(decls, calls, Shell.params.opt_WALAFileName)
      else MHashMap[(Any, Any), Int]()

    val inputTime = (System.nanoTime - inputstart) / 1000000000.0
    eprintln("# Time for parse the call history information(s): %.2f\n".format(inputTime))

    // Initialize features.
    val feature_map: HashMap[(Any, Any), List[Int]] =
      init_set(init_map) >>
        Classifier.genFeature >>
        SimpleName.genFeature >>
        PropName.genFeature(PropName.init(disambiguatedProgram)) >>
        OneshotCall.genFeature(OneshotCall.init(disambiguatedProgram))

    val outputstart = System.nanoTime
    if (Shell.params.opt_OutFileName != null) {
      val pw = new PrintWriter(new File(Shell.params.opt_OutFileName))

      calls.foreach(call => {
        decls.foreach(decl => {
          val bitvectors: List[Int] = feature_map((decl, call))
          if (Shell.params.opt_debug) {
            pw.write(string(call) + " => " + string(decl) + "\t")
          }
          bitvectors.foreach(v => pw.write(v + " "))
          pw.write(":")
          val answer = result_map((decl, call))
          pw.write(answer.toString)
          val wala = wala_map.get((decl, call)) match {
            case Some(v) => v.toString
            case _ => ""
          }
          pw.write(" "+wala)
          pw.write("\r\n")
        })
      })

      pw.close()
    } else {
      eprintln("* data")
      calls.foreach(call => {
        decls.foreach(decl => {
          val bitvectors: List[Int] = feature_map((decl, call))
          if (Shell.params.opt_debug) {
            eprint(string(call) + " => " + string(decl) + "\t")
          }
          bitvectors.foreach(v => System.out.print(v + " "))
          System.out.print(":")
          val answer = result_map((decl, call))
          System.out.println(answer)
          val wala = wala_map.get((decl, call)) match {
            case Some(v) => v.toString
            case _ => ""
          }
          System.out.println(" "+wala)
        })
      })
    }
    val outputTime = (System.nanoTime - outputstart) / 1000000000.0
    eprintln("# Time for printing out the result(s): %.2f\n".format(outputTime))
    val totalTime = (System.nanoTime - start) / 1000000000.0
    eprintln("# Total time(s): %.2f\n".format(totalTime))

    return_code
  }
}
