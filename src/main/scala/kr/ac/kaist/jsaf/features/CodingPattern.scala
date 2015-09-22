package kr.ac.kaist.jsaf.features

import scala.collection.immutable.HashMap

/**
 * Created by ysko on 15. 8. 3.
 */
object CodingPattern extends Features {
  override def featureName: String = "Coding pattern features"

  def genFeature(df: HashMap[Any, List[Int]], cf: HashMap[Any, List[Int]])(map: FeatureMap) = {
    genFeatureInit()

    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2
      val decl = dc._1
      val call = dc._2

      val dv: List[Int] = df(decl)
      val cv: List[Int] = cf(call)

      (dc, dv ++ cv ++ vectors)
    })

    genFeatureFinish()

    m
  }
}
