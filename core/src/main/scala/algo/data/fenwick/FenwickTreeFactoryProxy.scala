package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

trait FenwickTreeFactoryProxy[
    -E[V] <: CommutativeSemigroup[V],
    +Collection[K, V] <: SemigroupFenwickTreeOps[K, V, Collection[K, V]]
] extends FenwickTreeFactory[E, Collection] {

  protected def factory: FenwickTreeFactory[E, Collection]

  override def from[V: E](iterable: IterableOnce[V]): Collection[Int, V] =
    factory.from(iterable)

}
