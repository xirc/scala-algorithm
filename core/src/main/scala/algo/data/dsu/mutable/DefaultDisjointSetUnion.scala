package algo.data.dsu.mutable

import algo.data.dsu.DisjointSetUnionNode
import cats.kernel.CommutativeSemigroup

import scala.collection.mutable.ArrayBuffer

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup](
    values: IterableOnce[V]
) extends DisjointSetUnion[V] {

  private val nodes: ArrayBuffer[DisjointSetUnionNode[V]] = {
    values.iterator.zipWithIndex
      .map { case (value, index) =>
        DisjointSetUnionNode(index, 0, value)
      }
      .to(ArrayBuffer)
  }

  override def size: Int = nodes.size

  override def apply(v: Int): V = findNode(v).value

  override def find(v: Int): V = findNode(v).value

  override def isSame(u: Int, v: Int): Boolean = {
    findNode(u).leader == findNode(v).leader
  }

  override def unite(u: Int, v: Int): this.type = {
    val lu = findNode(u).leader
    val lv = findNode(v).leader
    if (lu == lv) {
      this
    } else {
      val (leader, follower) =
        if (nodes(lu).rank > nodes(lv).rank) (lu, lv) else (lv, lu)
      val (newLeader, newFollower) =
        DisjointSetUnionNode.unite(nodes(leader), nodes(follower))
      nodes(follower) = newFollower
      nodes(leader) = newLeader
      this
    }
  }

  private def findNode(v: Int): DisjointSetUnionNode[V] = {
    if (v < 0 || v >= size)
      throw new IndexOutOfBoundsException(s"Index out of range [0, $size): $v")
    if (v != nodes(v).leader) {
      // Path Compression
      nodes(v) = findNode(nodes(v).leader)
    }
    nodes(v)
  }

}
