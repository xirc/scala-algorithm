package algo.data.fenwick.mutable

import cats.kernel.CommutativeSemigroup

import scala.collection.mutable

private object SemigroupFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[
      CommutativeSemigroup,
      SemigroupFenwickTree
    ] {

  override protected def apply[V: CommutativeSemigroup](
      values: mutable.ArrayBuffer[V]
  ): SemigroupFenwickTreeDense1D[V] =
    new SemigroupFenwickTreeDense1D(values, implicitly[CommutativeSemigroup[V]])

}
