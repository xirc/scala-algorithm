package algo.data.fenwick.immutable

import cats.kernel.CommutativeSemigroup

import scala.collection.immutable

private final class SemigroupFenwickTreeDense1D[V](
    _values: immutable.Vector[V],
    ev: CommutativeSemigroup[V]
) extends SemigroupFenwickTree[Int, V]
    with SemigroupFenwickTreeOps[Int, V, SemigroupFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[
      V,
      SemigroupFenwickTreeDense1D[V]
    ] {

  override implicit def semigroup: CommutativeSemigroup[V] = ev

  override def size: Int = _values.size

  override protected def build(
      values: immutable.Vector[V]
  ): SemigroupFenwickTreeDense1D[V] =
    new SemigroupFenwickTreeDense1D(values, semigroup)

  override protected def underlyingCollection: immutable.Vector[V] =
    _values

}
