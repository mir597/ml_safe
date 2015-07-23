/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import kr.ac.kaist.jsaf.features.SimpleName
import kr.ac.kaist.jsaf.ml.CallHistoryParser
import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.immutable.HashMap
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf._
import kr.ac.kaist.jsaf.features._

////////////////////////////////////////////////////////////////////////////////
// Analyze
////////////////////////////////////////////////////////////////////////////////
object AnalyzeMain {

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
    decls.foreach (n => System.err.println("- "+string(n)))

    System.err.println("** Calls **")
    calls.foreach (n => System.err.println("- "+string(n)))

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
    val result_map = CallHistoryParser.parseFromFile(decls, calls, Shell.params.opt_ResultFileName)

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
        System.err.print(string(call) + " => " + string(decl)+ "\t")
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
