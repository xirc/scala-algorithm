package algo.data.fenwick.immutable

import cats.kernel.CommutativeSemigroup

import scala.collection.immutable

private object SemigroupFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[
      CommutativeSemigroup,
      SemigroupFenwickTree
    ] {

  override protected def apply[V: CommutativeSemigroup](
      values: immutable.Vector[V]
  ): SemigroupFenwickTree[Int, V] =
    new SemigroupFenwickTreeDense1D[V](
      values,
      implicitly[CommutativeSemigroup[V]]
    )

}
