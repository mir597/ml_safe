package kr.ac.kaist

import kr.ac.kaist.jsaf.nodes_util.Span
import kr.ac.kaist.jsaf.scala_src.nodes.{SNew, SFunApp, SFunExpr, SFunDecl}

/**
 * Created by ysko on 15. 7. 23..
 */
package object jsaf {
  implicit def any2waterfall[A](a: A): Object {def >>[B](f: (A) => B): B} = new AnyRef{
    def >>[B](f: A=>B) = f(a)
  }

  def abbreviate(s: String, len: Int): String = {
    val i = s.replace("\n", " ").replace("\r", "")
    if (i.length < len) i
    else i.substring(0, len - 4) + " ..."
  }

  def span(s: Span): String = {
    val fn = s.getFileNameOnly
    val sn = s.getBegin
    val en = s.getEnd

    val os = sn.getOffset + "~" + (en.getOffset - sn.getOffset)

    fn + "@" + os
  }

//  def span(i: Span): String = {
//    val begin = i.getBegin
//    val end = i.getEnd
//    begin.getFileNameOnly + "@" + begin.getLine +":" + begin.column() + "~" + end.getLine + ":" + end.column()
//  }

  def string(n: Any) = n match {
    case SFunDecl(info, ftn, strict) =>
      span(info.getSpan)+": "+ftn.getName.getText
    case SFunExpr(info, ftn) =>
      span(info.getSpan)+": "+ftn.getName.getText
    case s@SFunApp(info, fun, args) =>
      span(info.getSpan)
    case s@SNew(info, lhs) =>
      span(info.getSpan)
  }
}

