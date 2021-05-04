package algo.data.dsu.immutable

import cats.kernel.CommutativeSemigroup
import cats.syntax.semigroup._

private object DefaultDisjointSetUnion {

  final case class Node[V: CommutativeSemigroup](
      leader: Int,
      rank: Int,
      value: V
  )

}

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup](
    val nodes: Vector[DefaultDisjointSetUnion.Node[V]]
) extends DisjointSetUnion[V] {
  import DefaultDisjointSetUnion._

  def this(values: IterableOnce[V]) = {
    this(
      values.iterator.zipWithIndex.map { case (value, index) =>
        DefaultDisjointSetUnion.Node(index, 0, value)
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
      return this
    }

    val (leader, follower) =
      if (nodes(lu).rank > nodes(lv).rank) (lu, lv) else (lv, lu)

    val newFollower = nodes(follower).copy(leader = leader)
    nodes = nodes.updated(follower, newFollower)

    val newLeadersValue =
      nodes(leader).value |+| nodes(follower).value
    val newLeadersRank = {
      if (nodes(leader).rank == nodes(follower).rank) {
        nodes(leader).rank + 1
      } else {
        nodes(leader).rank
      }
    }
    val newLeader =
      nodes(leader).copy(rank = newLeadersRank, value = newLeadersValue)
    nodes = nodes.updated(leader, newLeader)

    new DefaultDisjointSetUnion(nodes)
  }

  private def compress(
      nodes: Vector[Node[V]],
      indice: Int*
  ): Vector[Node[V]] = {
    indice.foldLeft(nodes) { (nodes, index) =>
      compress(nodes, index)
    }
  }

  private def compress(nodes: Vector[Node[V]], v: Int): Vector[Node[V]] = {
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
