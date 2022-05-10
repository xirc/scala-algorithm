package algo.data.fenwick

import cats.kernel.CommutativeMonoid

trait FenwickTreeSparseFactoryProxy[
    -E[V] <: CommutativeMonoid[V],
    +Collection[_, _]
] extends FenwickTreeSparseFactory[E, Collection] {

  protected def sparseFactory: FenwickTreeSparseFactory[E, Collection]

  override def from[V: E](
      size: Long,
      iterable: IterableOnce[(Long, V)]
  ): Collection[Long, V] =
    sparseFactory.from(size, iterable)

  override def from[V: E](
      size1: Long,
      size2: Long,
      iterable: IterableOnce[((Long, Long), V)]
  ): Collection[(Long, Long), V] =
    sparseFactory.from(size1, size2, iterable)

}
