package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.nodes.VarRef
import kr.ac.kaist.jsaf.scala_src.nodes.{SNew, SFunApp}

/**
 * Created by ysko on 15. 7. 23..
 */
object Classifier extends Features {
  override def featureName: String = "Simple Classifier"

  def genFeature(map: FeatureMap) = {
    genFeatureInit()
    // classify the type of call expressions.
    // 1: simple function call
    // 2: method call
    val m = map.map(f => {
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

    genFeatureFinish()
    m
  }
}
