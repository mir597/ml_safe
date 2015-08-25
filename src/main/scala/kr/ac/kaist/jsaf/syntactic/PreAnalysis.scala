package kr.ac.kaist.jsaf.syntactic

import scala.collection.immutable.{HashSet, HashMap}

/**
 * Created by ysko on 15. 7. 25..
 */
trait PreAnalysis {
  var start = 0L
  type Callgraph = HashMap[Any, HashSet[Any]]
  val emptyGraph = HashMap[Any, HashSet[Any]]()

  protected def mergeCG(cg1: Callgraph, cg2: Callgraph): Callgraph = {
    (cg1 /: cg2)((cg, ce) => cg + (ce._1 -> (cg.getOrElse(ce._1, HashSet[Any]()) ++ ce._2)))
  }

  def featureName: String
  def genFeatureInit() = {
    start = System.nanoTime
  }

  def genFeatureFinish() = {
    val time = (System.nanoTime - start) / 1000000000.0
    System.err.println("# Time for "+ featureName +"(s): %.2f\n".format(time))
  }
}
