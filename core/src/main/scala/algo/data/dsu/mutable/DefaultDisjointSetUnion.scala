package algo.data.dsu.mutable

import cats.kernel.CommutativeSemigroup
import cats.syntax.semigroup._

import scala.collection.mutable.ArrayBuffer

private object DefaultDisjointSetUnion {

  final case class Node[V: CommutativeSemigroup](
      leader: Int,
      rank: Int,
      value: V
  )

}

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup](
    values: IterableOnce[V]
) extends DisjointSetUnion[V] {
  import DefaultDisjointSetUnion._

  private val nodes: ArrayBuffer[Node[V]] = {
    values.iterator.zipWithIndex
      .map { case (value, index) =>
        Node(index, 0, value)
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
      return this
    }

    val (leader, follower) =
      if (nodes(lu).rank > nodes(lv).rank) (lu, lv) else (lv, lu)

    val newFollower = nodes(follower).copy(leader = leader)
    nodes(follower) = newFollower

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
    nodes(leader) = newLeader

    this
  }

  private def findNode(v: Int): Node[V] = {
    if (v < 0 || v >= size)
      throw new IndexOutOfBoundsException(s"Index out of range [0, $size): $v")
    if (v != nodes(v).leader) {
      // Path Compression
      nodes(v) = findNode(nodes(v).leader)
    }
    nodes(v)
  }

}
