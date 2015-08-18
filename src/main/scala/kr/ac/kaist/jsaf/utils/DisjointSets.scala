package kr.ac.kaist.jsaf.utils

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}

/**
 * This class implements a disjoint-sets algorithm with
 * union-by-rank and path compression. The forest/sets/etc. are
 * internal data structures not exposed to the outside. Instead,
 * it is possible to add new elements (which end up in a newly
 * created set), union two elements, and hence, their sets, and
 * find the representative of a disjoint-set by a given element of
 * the set.
 */
class DisjointSets[T](initialElements : Seq[T] = Nil) {

  /**
   * Add a new single-node forest to the disjoint-set forests. It will
   * be placed into its own set.
   */
  def add(elem : T) : Unit = synchronized {
    nodes += (elem -> DisjointSets.Node(elem, 0, None))
  }

  /**
   * Union the disjoint-sets of which <code>elem1</code>
   * and <code>elem2</code> are members of.
   * @return the representative of the unioned set
   * @precondition elem1 and elem2 must have been added before
   */
  def union(elem1 : T, elem2 : T) : T = synchronized {
    // lookup elements
    if (!nodes.contains(elem1)) add(elem1)
    if (!nodes.contains(elem2)) add(elem2)
    require(nodes.contains(elem1) && nodes.contains(elem2),
      "Only elements previously added to the disjoint-sets can be unioned")

    // retrieve representative nodes
    (nodes.get(elem1).map(_.getRepresentative),
      nodes.get(elem2).map(_.getRepresentative)) match {
      // Distinguish the different union cases and return the new set representative

      // Case #1: both elements already in same set
      case (Some(n1), Some(n2)) if n1 == n2 =>
        n1.elem

      // Case #2: rank1 > rank2 -> make n1 parent of n2
      case (Some(n1 @ DisjointSets.Node(_, rank1, _)),
      Some(n2 @ DisjointSets.Node(_, rank2, _))) if rank1 > rank2 =>
        n2.parent = Some(n1)
        n1.elem

      // Case #3: rank1 < rank2 -> make n2 parent of n1
      case (Some(n1 @ DisjointSets.Node(_, rank1, _)),
      Some(n2 @ DisjointSets.Node(_, rank2, _))) if rank1 < rank2 =>
        n1.parent = Some(n2)
        n2.elem

      // Case #4: rank1 == rank2 -> keep n1 as representative and increment rank
      case (Some(n1 @ DisjointSets.Node(_, rank1, _)),
      Some(n2 @ DisjointSets.Node(_, rank2, _))) /*if rank1 == rank2*/ =>
        n1.rank = rank1 + 1
        n2.parent = Some(n1)
        n1.elem

      // we are guaranteed to find the two nodes in the map,
      // and the above cases cover all possibilities
      case _ => throw new MatchError("This should never have happened")
    }
  }

  /**
   * Finds the representative for a disjoint-set, of which
   * <code>elem</code> is a member of.
   */
  def find(elem : T) : Option[T] = synchronized {
    nodes.get(elem) match {
      case Some(node) =>
        val rootNode = node.getRepresentative
        // path compression
        if (node != rootNode) node.parent = Some(rootNode)
        Some(rootNode.elem)
      case None => None
    }
  }

  /**
   * Returns the number of disjoint-sets managed in this data structure.
   * Keep in mind: this is a non-vital/non-standard operation, so we do
   * not keep track of the number of sets, and instead this method recomputes
   * them each time.
   */
  def size : Int = synchronized {
    nodes.values.count(_.parent == None)
  }

  def foreach(f: T => Unit) = nodes.foreach(v => f(v._1))
  def foldLeft[A](s: A)(f: (A, T) => A) = (s /: nodes)((a, t) => f(a, t._1))
  def /:[B](z: B)(op: (B, T) => B): B = foldLeft(z)(op)
  def groups: List[Set[T]] = {
    val map = HashMap[T, HashSet[T]]()
    val emptyset = HashSet[T]()
    val nmap =
      (map /: this)((map_i, e) => {
        val r = this.find(e).get
        map_i + (r -> (map_i.getOrElse(r, emptyset) + e))
      })

    (List[Set[T]]() /: nmap)((list, s) => s._2::list)
  }

  ////
  // Internal parts
  private val nodes : scala.collection.mutable.Map[T, DisjointSets.Node[T]] =
    scala.collection.mutable.Map.empty

  // Initialization
  synchronized { initialElements map (add _) }
}

object DisjointSets {
  def apply[T](initialElements : Seq[T] = Nil) = new DisjointSets[T](initialElements)

  ////
  // Internal parts
  private case class Node[T](val elem : T, var rank : Int, var parent : Option[Node[T]]) {
    /**
     * Compute representative of this set.
     * @return root element of the set
     */
    @tailrec
    final def getRepresentative: Node[T] = parent match {
      case None => this
      case Some(p) => p.getRepresentative
    }
  }
}