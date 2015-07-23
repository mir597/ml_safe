package kr.ac.kaist.jsaf.features

import scala.collection.immutable.HashMap

/**
 * Created by ysko on 15. 7. 23..
 */
object SimpleName {
  def genFeature(map: HashMap[(Any, Any), List[Int]]) = {
    map.map(f => {
      val dc = f._1
      val vectors = f._2
      val callname = name(dc._2)
      val declname = name(dc._1)

      val vec =
        if (callname.equals(declname) && !callname.equals("")) 1
        else 0

      (dc, vec::vectors)
    })
  }
}
