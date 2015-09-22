package kr.ac.kaist.jsaf

import kr.ac.kaist.jsaf.nodes.Id

/**
 * Created by ysko on 15. 7. 23..
 */
package object features {
  type Entity = String
//  sealed trait Entity {
//    val id: Int
//    def comp(that: Entity): Int
//    def comptext(that: Entity): Boolean
//
//    def isEmpty: Boolean
//  }
//  case class Name(_uniqueID: String) extends Entity {
//    val id = 1
//    val uniqueID = _uniqueID
//    def comptext(that: Entity): Boolean = {
//      this._uniqueID == that.asInstanceOf[Name].uniqueID
//    }
//    def comp(that: Entity): Int = {
//      this.uniqueID compare that.asInstanceOf[Name].uniqueID
//    }
//
//    def isEmpty: Boolean = _uniqueID.contains(" ") || _uniqueID.equals("") || _uniqueID == null
//
//    override def toString: String = _uniqueID
//  }
//  implicit object order_entity extends Ordering[Entity] {
//    override def compare(x: Entity, y: Entity): Int = x.comp(y)
//  }

//  def nid_(id: Id): Entity = new Name(id.getUniqueName.unwrap())
//  def nid(id: String): Entity = new Name(id)
  def nid_(id: Id): Entity = id.getUniqueName.unwrap()
  def nid(id: String): Entity = id
}
