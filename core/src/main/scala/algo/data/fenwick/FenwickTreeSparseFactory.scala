package algo.data.fenwick

import cats.kernel.CommutativeMonoid

trait FenwickTreeSparseFactory[
    -E[V] <: CommutativeMonoid[V],
    +Collection[K, V]
] {

  def from[V: E](
      size: Long,
      iterable: IterableOnce[(Long, V)]
  ): Collection[Long, V]

  def from[V: E](
      size1: Long,
      size2: Long,
      iterable: IterableOnce[((Long, Long), V)]
  ): Collection[(Long, Long), V]

  def apply[V: E](size: Long, xs: (Long, V)*): Collection[Long, V] =
    from(size, xs)

  def apply[V: E](
      size1: Long,
      size2: Long,
      xs: ((Long, Long), V)*
  ): Collection[(Long, Long), V] =
    from(size1, size2, xs)

}
