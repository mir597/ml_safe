package kr.ac.kaist.jsaf.features

import kr.ac.kaist.jsaf._
import kr.ac.kaist.jsaf.nodes.Id
import kr.ac.kaist.jsaf.scala_src.nodes._

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ HashMap => MHashMap }

/**
 * Created by ysko on 15. 9. 21..
 */
object IDFeatures {
  type t = HashMap[Entity, IDFeature]
  val VarRef = "VarRef"
  val FEName = "FEName"
  val FEArgs = "FEArgs"
  val FDName = "FDName"
  val FDArgs = "FDArgs"
  val Prop = "Prop"

  class IDFeature {
    val map = MHashMap[String, Int]()

    private def u(s: (String, Int)) = map += s
    def asVarRef() = u(VarRef -> 1)
    def asFunExprName() = u(FEName -> 1)
    def asFunExprArgs(ith: Int) = u(FEArgs -> (ith+1))
    def asFunDeclName() = u(FDName -> 1)
    def asFunDeclArgs(ith: Int) = u(FDArgs -> (ith+1))
    def asProp() = u(Prop -> 1)

    private def g(n: String) = map.getOrElse(n, 0)

    override def toString: String = {
      (g(VarRef)::g(FEName)::g(FEArgs)::g(FDName)::g(FDArgs)::g(Prop)::Nil).mkString(" ")
    }

    def toVectors: List[Int] = g(VarRef)::g(FEName)::g(FEArgs)::g(FDName)::g(FDArgs)::g(Prop)::Nil
  }

  val empty = new IDFeature()

  def add(id: Entity, f: IDFeature => Unit)(map: HashMap[Entity, IDFeature]): HashMap[Entity, IDFeature] = {
    val feature = map.getOrElse(id, new IDFeature())
    f(feature)
    map + (id -> feature)
  }
  implicit def inid(id: Id): Entity = nid_(id)
  implicit def snid(s: String): Entity = nid(s)

  def collectIDFeatures(parent: Any, node: Any, map: HashMap[Entity, IDFeature]): HashMap[Entity, IDFeature] = {
    node match {
      case SVarRef(_, n) => map >> add(n, _.asVarRef())
      case SFunExpr(_, SFunctional(_, _, _, name, params)) =>
        params.zipWithIndex.foldLeft(map)((m, p) => add(p._1, _.asFunExprArgs(p._2))(m)) >>
          add(name, _.asFunExprName())
      case SFunDecl(_, SFunctional(_, _, _, name, params), _) =>
        params.zipWithIndex.foldLeft(map)((m, p) => add(p._1, _.asFunDeclArgs(p._2))(m)) >>
          add(name, _.asFunDeclName())
      case SPropId(_, id) => map >> add(id.getText, _.asProp())
      case SPropStr(_, str) => map >> add(str, _.asProp())

      case _ => map
    }
  }

  def genFeatures(pgm: Any): t = {
    walkAST(collectIDFeatures)(null, pgm)(HashMap[Entity, IDFeature]())
  }

  def find(t: t, id: Option[Entity]): List[Int] = {
    id match {
      case Some(e) =>
        t.get(e) match {
          case Some(v) => v.toVectors
          case _ => empty.toVectors
        }
//        t(e).toVectors

      case None => empty.toVectors
    }
  }
}
