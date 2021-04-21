package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

trait SemigroupFenwickTree[K, V]
    extends SemigroupFenwickTreeOps[K, V, SemigroupFenwickTree[K, V]]

object SemigroupFenwickTree
    extends FenwickTreeFactoryProxy[
      CommutativeSemigroup,
      SemigroupFenwickTree
    ] {

  override protected def factory
      : FenwickTreeFactory[CommutativeSemigroup, SemigroupFenwickTree] =
    SemigroupFenwickTreeFactory

}
