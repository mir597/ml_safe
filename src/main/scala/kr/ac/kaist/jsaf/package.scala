package kr.ac.kaist

import kr.ac.kaist.jsaf.nodes_util.JSAstToConcrete
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

  def string(n: Any) = n match {
    case SFunDecl(info, ftn, strict) =>
      info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles+": "+ftn.getName.getText
    case SFunExpr(info, ftn) =>
      info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles+": "+ftn.getName.getText
    case s@SFunApp(info, fun, args) =>
      val str = abbreviate(JSAstToConcrete.walk(fun), 20)
      info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles + ": " + str
    case s@SNew(info, lhs) =>
      val str = abbreviate(JSAstToConcrete.walk(lhs), 20)
      info.getSpan.getFileNameOnly+"@"+info.getSpan.toStringWithoutFiles + ": " + str
  }
}

