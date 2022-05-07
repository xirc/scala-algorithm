package algo.data.dsu.mutable

import algo.data.dsu.DisjointSetUnionNode
import cats.kernel.CommutativeSemigroup

import java.util.ConcurrentModificationException
import scala.collection.AbstractIterator
import scala.collection.immutable.IntMap
import scala.collection.mutable.ArrayBuffer

private final class DefaultDisjointSetUnion[V: CommutativeSemigroup] private (
    val nodes: ArrayBuffer[DisjointSetUnionNode[V]],
    var _groupCount: Int
) extends DisjointSetUnion[V] { self =>

  @transient private var mutationCount: Int = 0

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
      _groupCount -= 1
      mutationCount += 1
      this
    }
  }

  override def groupCount: Int = _groupCount

  override def groups: Set[Set[Int]] = {
    val _groups = (0 until size).foldLeft(IntMap.empty[Set[Int]]) {
      (_groups, index) =>
        val leader = findNode(index).leader
        val group = _groups.getOrElse(leader, Set.empty)
        _groups + (leader -> (group + index))
    }
    _groups.values.toSet
  }

  override def iterator: Iterator[V] = new AbstractIterator[V] {
    private val mutationCountAtCreation: Int = mutationCount
    private var index: Int = 0
    override def knownSize: Int = self.size - index
    override def hasNext: Boolean = {
      if (mutationCount != mutationCountAtCreation) {
        throw new ConcurrentModificationException(
          "Mutation occurred during iteration."
        )
      }
      index < self.size
    }
    override def next(): V = {
      val value = self.find(index)
      index += 1
      value
    }
  }

  override def knownSize: Int = size

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

private object DefaultDisjointSetUnion {

  def apply[V: CommutativeSemigroup](
      values: IterableOnce[V]
  ): DefaultDisjointSetUnion[V] = {
    val buffer = values.iterator.zipWithIndex
      .map { case (value, index) =>
        DisjointSetUnionNode(index, 0, value)
      }
      .to(ArrayBuffer)
    new DefaultDisjointSetUnion(buffer, buffer.size)
  }

}
