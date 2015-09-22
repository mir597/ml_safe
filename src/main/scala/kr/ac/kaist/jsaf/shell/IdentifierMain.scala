/******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.shell

import java.io.{File, PrintWriter}

import kr.ac.kaist.jsaf.{Shell, _}
import kr.ac.kaist.jsaf.compiler.{Disambiguator, Hoister, Parser}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.nodes.{Id, Program}
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.JavaConversions
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable.{HashMap => MHashMap}
import scala.io.Source

////////////////////////////////////////////////////////////////////////////////
// Analyze
////////////////////////////////////////////////////////////////////////////////
object IdentifierMain {
  final private val usedOnly = true

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
    val pgm = new Disambiguator(hoistedProgram, disambiguateOnly = false).doit().asInstanceOf[Program]
    val postTime = (System.nanoTime - poststart) / 1000000000.0
    eprintln("# Time for hoisting and disambiguation(s): %.2f\n".format(postTime))

    sealed trait Entity {
      val id: Int
      def comp(that: Entity): Int
      def comptext(that: Entity): Boolean

      def isEmpty: Boolean
    }
    case class Name(_text: String, _uniqueID: String) extends Entity {
      val id = 1
      val text = _text
      val uniqueID = _uniqueID
      def comptext(that: Entity): Boolean = {
        if (this.id - that.id != 0) false
        else this.text.equals(that.asInstanceOf[Name].text)
      }
      def comp(that: Entity): Int = {
        val oid = this.id - that.id
        if (oid != 0) oid
        else this.uniqueID compare that.asInstanceOf[Name].uniqueID
      }

      def isEmpty: Boolean = _uniqueID.contains(" ") || _uniqueID.equals("") || _uniqueID == null

      override def toString: String = _uniqueID
    }
    implicit object order_entity extends Ordering[Entity] {
      override def compare(x: Entity, y: Entity): Int = x.comp(y)
    }

    def nid_(id: Id): Entity = new Name(id.getText, id.getUniqueName.unwrap)
    def nid(id: String): Entity = new Name(id, id)

    val number_literal: String = "$*Number*$"

    if (Shell.params.opt_OutFileName != null) {
      val pw = new PrintWriter(new File(Shell.params.opt_OutFileName))

      def collectIdentifiers(parent: Any, node: Any, set: HashSet[Entity]) = node match {
        case n: Id => set + nid_(n)
        case SPropStr(_, str) => set + nid(str)
        case SPropNum(_, _) => set + nid(number_literal)
        case SStringLiteral(_, _, s) => set + nid(s)
        case SIntLiteral(_, _, _) => set + nid(number_literal)
        case SDoubleLiteral(_, s, _) => set + nid(number_literal)
        case SBool(_, tf) => set + nid(tf.toString)
        case SNull(_) => set + nid("null")
        case _ => set
      }

      val initstart = System.nanoTime
      val ids = walkAST(collectIdentifiers)(null, pgm)(HashSet())
      val initTime = (System.nanoTime - initstart) / 1000000000.0
      eprintln("# Time for extracting function decl and call exprs(s): %.2f\n".format(initTime))

      val outputstart = System.nanoTime

      eprintln("* data")
      var i = 0
      def newI() = {
        val j = i
        i = i + 1
        j
      }
      ids.filter(!_.isEmpty).foreach(ef => {
        pw.write(newI() + " " + ef+"\n")
      })
      pw.close()
      val outputTime = (System.nanoTime - outputstart) / 1000000000.0
      eprintln("# Time for printing out the result(s): %.2f\n".format(outputTime))
      val totalTime = (System.nanoTime - start) / 1000000000.0
      eprintln("# Total time(s): %.2f\n".format(totalTime))
    } else if (Shell.params.opt_LoadFileName != null) {

//      val map = (HashMap[Entity, Int]() /: Source.fromFile(Shell.params.opt_LoadFileName).getLines())((m, line) => {
//        val Array(i, id) = line.split(' ')
//        m + (nid(id) -> i.toInt)
//      })

      class IDFeature {
        val map = MHashMap[String, Int]()
        val VarRef = "VarRef"
        val FEName = "FEName"
        val FEArgs = "FEArgs"
        val FDName = "FDName"
        val FDArgs = "FDArgs"
        val Prop = "Prop"

        private def u(s: (String, Int)) = map += s
        def asVarRef() = u(VarRef -> 1)
        def asFunExprName() = u(FEName -> 1)
        def asFunExprArgs(ith: Int) = u(FEArgs -> (ith+1))
        def asFunDeclName() = u(FDName -> 1)
        def asFunDeclArgs(ith: Int) = u(FDArgs -> (ith+1))
        def asProp() = u(Prop -> 1)

        private def g(n: String) = map.getOrElse(n, 0)

        override def toString: String = {
          (g(VarRef)::g(FEName)::g(FEArgs)::g(FDName)::g(FDArgs)::g(Prop)::Nil).mkString(" ")
        }
      }

      def add(id: Entity, f: IDFeature => Unit)(map: HashMap[Entity, IDFeature]): HashMap[Entity, IDFeature] = {
        val feature = map.getOrElse(id, new IDFeature())
        f(feature)
        map + (id -> feature)
      }
      implicit def inid(id: Id): Entity = nid_(id)
      implicit def snid(s: String): Entity = nid(s)

      def collectIDFeatures(parent: Any, node: Any, map: HashMap[Entity, IDFeature]): HashMap[Entity, IDFeature] = {
        node match {
          case SVarRef(_, n) => map >> add(n, _.asVarRef())
          case SFunExpr(_, SFunctional(_, _, _, name, params)) =>
            params.zipWithIndex.foldLeft(map)((m, p) => add(p._1, _.asFunExprArgs(p._2))(m)) >>
              add(name, _.asFunExprName())
          case SFunDecl(_, SFunctional(_, _, _, name, params), _) =>
            params.zipWithIndex.foldLeft(map)((m, p) => add(p._1, _.asFunDeclArgs(p._2))(m)) >>
            add(name, _.asFunDeclName())
          case SPropId(_, id) => map >> add(id, _.asProp())
          case SPropStr(_, str) => map >> add(str, _.asProp())

          case _ => map
        }
      }

      val features = walkAST(collectIDFeatures)(null, pgm)(HashMap[Entity, IDFeature]())
      features.foreach(f => System.out.println(f._1+" - "+f._2))
    }

    return_code
  }
}
