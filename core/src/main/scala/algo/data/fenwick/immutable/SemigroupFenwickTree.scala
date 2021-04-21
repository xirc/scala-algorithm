package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.data.fenwick.{FenwickTreeFactory, FenwickTreeFactoryProxy}
import cats.kernel.CommutativeSemigroup

trait SemigroupFenwickTree[K, V]
    extends fenwick.SemigroupFenwickTree[K, V]
    with SemigroupFenwickTreeOps[K, V, SemigroupFenwickTree[K, V]]

object SemigroupFenwickTree
    extends FenwickTreeFactoryProxy[
      CommutativeSemigroup,
      SemigroupFenwickTree
    ] {

  override protected def factory
      : FenwickTreeFactory[CommutativeSemigroup, SemigroupFenwickTree] =
    SemigroupFenwickTreeFactory

}
