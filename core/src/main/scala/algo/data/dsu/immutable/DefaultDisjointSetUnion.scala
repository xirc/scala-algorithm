package algo.data.dsu.immutable

import algo.data.dsu.DisjointSetUnionNode
import cats.kernel.CommutativeSemigroup

import scala.collection.AbstractIterator
import scala.collection.immutable.IntMap

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup] private (
    val nodes: Vector[DisjointSetUnionNode[V]],
    val _groupCount: Int
) extends DisjointSetUnion[V] {

  override def size: Int = nodes.size

  override def find(v: Int): (V, DisjointSetUnion[V]) = {
    throwIfOutOfBounds(v)
    val nodes = compress(this.nodes, v)
    (nodes(v).value, new DefaultDisjointSetUnion(nodes, groupCount))
  }

  override def isSame(u: Int, v: Int): (Boolean, DisjointSetUnion[V]) = {
    throwIfOutOfBounds(u)
    throwIfOutOfBounds(v)
    val nodes = compress(this.nodes, u, v)
    val hasSameLeader = nodes(u).leader == nodes(v).leader
    (hasSameLeader, new DefaultDisjointSetUnion(nodes, groupCount))
  }

  override def unite(u: Int, v: Int): DisjointSetUnion[V] = {
    throwIfOutOfBounds(u)
    throwIfOutOfBounds(v)
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
      new DefaultDisjointSetUnion(nodes, groupCount - 1)
    }
  }

  override def groupCount: Int = _groupCount

  override def groups: Set[Set[Int]] = {
    var newNodes = nodes
    val _groups = (0 until size).foldLeft(IntMap.empty[Set[Int]]) {
      (_groups, index) =>
        newNodes = compress(newNodes, index)
        val leader = newNodes(index).leader
        val group = _groups.getOrElse(leader, Set.empty)
        _groups + (leader -> (group + index))
    }
    _groups.values.toSet
  }

  override def iterator: Iterator[V] = new AbstractIterator[V] {
    private var self: DisjointSetUnion[V] = DefaultDisjointSetUnion.this
    private var index = 0
    override def knownSize: Int = self.size - index
    override def hasNext: Boolean = index < self.size
    override def next(): V = {
      val (value, newSelf) = self.find(index)
      self = newSelf
      index += 1
      value
    }
  }

  override def knownSize: Int = size

  @inline private def compress(
      nodes: Vector[DisjointSetUnionNode[V]],
      indices: Int*
  ): Vector[DisjointSetUnionNode[V]] = {
    indices.foldLeft(nodes) { (nodes, index) =>
      compress(nodes, index)
    }
  }

  private def compress(
      nodes: Vector[DisjointSetUnionNode[V]],
      v: Int
  ): Vector[DisjointSetUnionNode[V]] = {
    if (v != nodes(v).leader) {
      // Path Compression
      val leader = nodes(v).leader
      val compressed = compress(nodes, leader)
      nodes.updated(v, compressed(leader))
    } else {
      nodes
    }
  }

  @inline private def throwIfOutOfBounds(v: Int): Unit = {
    if (v < 0 || v >= size) {
      throw new IndexOutOfBoundsException(s"Index out of range [0, $size): $v")
    }
  }

}

private object DefaultDisjointSetUnion {

  def apply[V: CommutativeSemigroup](
      values: IterableOnce[V]
  ): DefaultDisjointSetUnion[V] = {
    val buffer = values.iterator.zipWithIndex.map { case (value, index) =>
      DisjointSetUnionNode(index, 0, value)
    }.toVector
    new DefaultDisjointSetUnion(buffer, buffer.size)
  }

}
