package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

private object SemigroupFenwickTreeFactory
    extends FenwickTreeFactory[
      CommutativeSemigroup,
      SemigroupFenwickTree
    ] {

  override def from[V: CommutativeSemigroup](
      iterable: IterableOnce[V]
  ): SemigroupFenwickTree[Int, V] =
    immutable.SemigroupFenwickTree.from(iterable)

}
