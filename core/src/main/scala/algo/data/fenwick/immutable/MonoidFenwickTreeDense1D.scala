package algo.data.fenwick.immutable

import algo.data.fenwick.MonoidFenwickTreeOpsDefaults
import cats.kernel.CommutativeMonoid

import scala.collection.immutable

private class MonoidFenwickTreeDense1D[V](
    _values: immutable.Vector[V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[Int, V]
    with MonoidFenwickTreeOps[Int, V, MonoidFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[V, MonoidFenwickTreeDense1D[V]]
    with MonoidFenwickTreeOpsDefaults[Int, V, MonoidFenwickTreeDense1D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: Int = _values.size

  override protected def build(
      values: immutable.Vector[V]
  ): MonoidFenwickTreeDense1D[V] =
    new MonoidFenwickTreeDense1D[V](values, monoid)

  override protected def underlyingCollection: immutable.Vector[V] =
    _values

}
