package kr.ac.kaist.jsaf.utils

import kr.ac.kaist.jsaf._
import kr.ac.kaist.jsaf.features._
import kr.ac.kaist.jsaf.nodes.Id
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.HashSet

/**
 * Created by ysko on 15. 9. 21..
 */
object IDCollector {

  private val number_literal: String = "$*Number*$"

  private def collectIdentifiers(parent: Any, node: Any, set: HashSet[Entity]) = node match {
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

  def collectIDs(pgm: Any): HashSet[Entity] = walkAST(collectIdentifiers)(null, pgm)(HashSet())

}
