package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf.features.PropName._
import kr.ac.kaist.jsaf.scala_src.nodes._
import kr.ac.kaist.jsaf.walkAST

import scala.collection.immutable.{HashMap, HashSet}

/**
 * Created by ysko on 15. 8. 3.
 */
object Syntactic extends Features {
  override def featureName: String = "Simple syntactic feature"

  def genFeature(funMap: OneshotCall.t)(maps: PropName.t)(map: FeatureMap) = {
    genFeatureInit()

    val m_1 = PropName.genMap(maps)(map)
    val m_2 = funMap

    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2
      val decl = dc._1
      val call = dc._2

      val p_vec = PropName.feature(m_1)(decl, call)
      val o_vec = OneshotCall.feature(m_2)(decl, call)

      val v = if (p_vec > 0 || o_vec > 0) 1 else 0
      (dc, v::vectors)
    })

    genFeatureFinish()

    m
  }
}
