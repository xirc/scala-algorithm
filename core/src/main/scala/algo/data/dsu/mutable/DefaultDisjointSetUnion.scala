package algo.data.dsu.mutable

import algo.data.dsu.DisjointSetUnionNode
import cats.kernel.CommutativeSemigroup

import scala.collection.mutable.ArrayBuffer

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup] private (
    val nodes: ArrayBuffer[DisjointSetUnionNode[V]]
) extends DisjointSetUnion[V] {

  def this(values: IterableOnce[V]) = {
    this(
      values.iterator.zipWithIndex
        .map { case (value, index) =>
          DisjointSetUnionNode(index, 0, value)
        }
        .to(ArrayBuffer)
    )
  }

  override def size: Int = nodes.size

  override def apply(v: Int): V = find(v)

  override def find(v: Int): V = {
    throwIfOutOfBounds(v)
    findNode(v).value
  }

  override def isSame(u: Int, v: Int): Boolean = {
    throwIfOutOfBounds(u)
    throwIfOutOfBounds(v)
    findNode(u).leader == findNode(v).leader
  }

  override def unite(u: Int, v: Int): this.type = {
    throwIfOutOfBounds(u)
    throwIfOutOfBounds(v)
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
    if (v != nodes(v).leader) {
      // Path Compression
      nodes(v) = findNode(nodes(v).leader)
    }
    nodes(v)
  }

  @inline private def throwIfOutOfBounds(v: Int): Unit = {
    if (v < 0 || v >= size) {
      throw new IndexOutOfBoundsException(s"Index out of range [0, $size): $v")
    }
  }

}
