package kr.ac.kaist.jsaf.features

import scala.collection.immutable.HashMap

/**
 * Created by ysko on 15. 7. 25..
 */
trait Features {
  type FeatureMap = HashMap[(Any, Any), List[Int]]
  var start = 0L

  def featureName: String
  def genFeatureInit() = {
    start = System.nanoTime
  }

  def genFeatureFinish() = {
    val time = (System.nanoTime - start) / 1000000000.0
    System.err.println("# Time for "+ featureName +"(s): %.2f\n".format(time))
  }
}
