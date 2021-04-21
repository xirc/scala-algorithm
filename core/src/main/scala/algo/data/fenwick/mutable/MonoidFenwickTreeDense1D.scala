package algo.data.fenwick.mutable

import algo.data.fenwick.MonoidFenwickTreeOpsDefaults
import cats.kernel.CommutativeMonoid

import scala.collection.mutable

private final class MonoidFenwickTreeDense1D[V](
    _values: mutable.ArrayBuffer[V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[Int, V]
    with MonoidFenwickTreeOps[Int, V, MonoidFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[V, MonoidFenwickTreeDense1D[V]]
    with MonoidFenwickTreeOpsDefaults[Int, V, MonoidFenwickTreeDense1D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: Int = _values.size

  override def clone(): MonoidFenwickTreeDense1D[V] =
    new MonoidFenwickTreeDense1D[V](_values.clone(), monoid)

  override protected def underlyingCollection: mutable.ArrayBuffer[V] =
    _values

}
