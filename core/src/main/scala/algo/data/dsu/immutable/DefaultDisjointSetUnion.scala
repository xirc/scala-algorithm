package algo.data.dsu.immutable

import algo.data.dsu.DisjointSetUnionNode
import cats.kernel.CommutativeSemigroup

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup](
    val nodes: Vector[DisjointSetUnionNode[V]]
) extends DisjointSetUnion[V] {

  def this(values: IterableOnce[V]) = {
    this(
      values.iterator.zipWithIndex.map { case (value, index) =>
        DisjointSetUnionNode(index, 0, value)
      }.toVector
    )
  }

  override def size: Int = nodes.size

  override def find(v: Int): (V, DisjointSetUnion[V]) = {
    val nodes = compress(this.nodes, v)
    (nodes(v).value, new DefaultDisjointSetUnion(nodes))
  }

  override def isSame(u: Int, v: Int): (Boolean, DisjointSetUnion[V]) = {
    val nodes = compress(this.nodes, u, v)
    val hasSameLeader = nodes(u).leader == nodes(v).leader
    (hasSameLeader, new DefaultDisjointSetUnion(nodes))
  }

  override def united(u: Int, v: Int): DisjointSetUnion[V] = {
    var nodes = compress(this.nodes, u, v)
    val lu = nodes(u).leader
    val lv = nodes(v).leader
    if (lu == lv) {
      this
    } else {
      val (leader, follower) =
        if (nodes(lu).rank >= nodes(lv).rank) (lu, lv) else (lv, lu)
      val (newLeader, newFollower) =
        DisjointSetUnionNode.unite(nodes(leader), nodes(follower))
      nodes = nodes.updated(follower, newFollower)
      nodes = nodes.updated(leader, newLeader)
      new DefaultDisjointSetUnion(nodes)
    }
  }

  private def compress(
      nodes: Vector[DisjointSetUnionNode[V]],
      indice: Int*
  ): Vector[DisjointSetUnionNode[V]] = {
    indice.foldLeft(nodes) { (nodes, index) =>
      compress(nodes, index)
    }
  }

  private def compress(
      nodes: Vector[DisjointSetUnionNode[V]],
      v: Int
  ): Vector[DisjointSetUnionNode[V]] = {
    if (v < 0 || v >= size)
      throw new IndexOutOfBoundsException(s"Index out of range [0, $size): $v")
    if (v != nodes(v).leader) {
      // Path Compression
      val leader = nodes(v).leader
      val compressed = compress(nodes, leader)
      nodes.updated(v, compressed(leader))
    } else {
      nodes
    }
  }

}
