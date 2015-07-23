package kr.ac.kaist.jsaf.ml

import kr.ac.kaist.jsaf.nodes_util.Span
import kr.ac.kaist.jsaf.scala_src.nodes.{SNew, SFunApp, SFunExpr, SFunDecl}
import net.liftweb.json.JsonAST.{JString, JArray, JField}
import net.liftweb.json._

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ HashMap => MHashMap }
import scala.io.Source

/**
 * Created by ysko on 15. 7. 23..
 */

object CallHistoryParser {
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
}
