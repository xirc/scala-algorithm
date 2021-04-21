package algo.data.fenwick.mutable

import cats.kernel.CommutativeGroup

import scala.collection.mutable

private final class GroupFenwickTreeDense1D[V](
    _values: mutable.ArrayBuffer[V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[Int, V]
    with GroupFenwickTreeOps[Int, V, GroupFenwickTreeDense1D[V]]
    with GroupFenwickTreeOpsDefaults[Int, V, GroupFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[V, GroupFenwickTreeDense1D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: Int = _values.size

  override def clone(): GroupFenwickTreeDense1D[V] =
    new GroupFenwickTreeDense1D(_values.clone(), group)

  override protected def underlyingCollection: mutable.ArrayBuffer[V] =
    _values

}
