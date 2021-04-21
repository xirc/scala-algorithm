package algo.data.fenwick.mutable

import algo.data.fenwick
import cats.kernel.CommutativeMonoid

private trait FenwickTreeSparseFactoryDefaults[
    -E[V] <: CommutativeMonoid[V],
    +Collection[K, V] <: MonoidFenwickTree[K, V]
] extends fenwick.FenwickTreeSparseFactory[E, Collection] {

  final override def from[V: E](
      size: Long,
      iterable: IterableOnce[(Long, V)]
  ): Collection[Long, V] = {
    if (size < 0)
      throw new IllegalArgumentException(s"size should be positive: $size")
    val tree = apply(size)
    for ((index, value) <- iterable.iterator) {
      tree.combine(index, value)
    }
    tree
  }

  final override def from[V: E](
      size1: Long,
      size2: Long,
      iterable: IterableOnce[((Long, Long), V)]
  ): Collection[(Long, Long), V] = {
    if (size1 < 0)
      throw new IllegalArgumentException(
        s"size1 should be non-negative: $size1"
      )
    if (size2 < 0)
      throw new IllegalArgumentException(
        s"size2 should be non-negative: $size2"
      )
    val tree = apply(size1, size2)
    iterable.iterator.foreach { case (index, value) =>
      tree.combine(index, value)
    }
    tree
  }

  protected def apply[V: E](size: Long): Collection[Long, V]

  protected def apply[V: E](
      size1: Long,
      size2: Long
  ): Collection[(Long, Long), V]

}
