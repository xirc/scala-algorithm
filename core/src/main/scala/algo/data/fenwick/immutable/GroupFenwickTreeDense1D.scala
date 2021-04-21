package algo.data.fenwick.immutable

import cats.kernel.CommutativeGroup

import scala.collection.immutable

private final class GroupFenwickTreeDense1D[V](
    _values: immutable.Vector[V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[Int, V]
    with GroupFenwickTreeOps[Int, V, GroupFenwickTreeDense1D[V]]
    with SemigroupFenwickTreeDenseOpsDefaults[V, GroupFenwickTreeDense1D[V]]
    with GroupFenwickTreeOpsDefaults[Int, V, GroupFenwickTreeDense1D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: Int = _values.size

  override protected def build(
      values: immutable.Vector[V]
  ): GroupFenwickTreeDense1D[V] =
    new GroupFenwickTreeDense1D(values, group)

  override protected def underlyingCollection: immutable.Vector[V] =
    _values

}
