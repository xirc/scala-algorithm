package algo.data.fenwick.mutable

import cats.kernel.CommutativeSemigroup

import scala.collection.mutable

private final class SemigroupFenwickTreeDense1D[V](
    _values: mutable.ArrayBuffer[V],
    ev: CommutativeSemigroup[V]
) extends SemigroupFenwickTree[Int, V]
    with SemigroupFenwickTreeOps[Int, V, SemigroupFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[
      V,
      SemigroupFenwickTreeDense1D[V]
    ] {

  override implicit def semigroup: CommutativeSemigroup[V] = ev

  override def size: Int = _values.size

  override def clone(): SemigroupFenwickTreeDense1D[V] =
    new SemigroupFenwickTreeDense1D(_values.clone(), semigroup)

  override protected def underlyingCollection: mutable.ArrayBuffer[V] =
    _values

}
