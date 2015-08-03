package kr.ac.kaist.jsaf.features

/**
 * Created by ysko on 15. 7. 23..
 */
object SimpleName extends Features {
  override def featureName: String = "Simple Name"

  def genFeature(map: FeatureMap) = {
    genFeatureInit()
    val m = map.map(f => {
      val dc = f._1
      val vectors = f._2
      val callname = name(dc._2)
      val declname = name(dc._1)

      val vec =
        if (callname.equals(declname) && !callname.equals("")) 1
        else 0

      (dc, vec::vectors)
    })

    genFeatureFinish()

    m
  }
}
