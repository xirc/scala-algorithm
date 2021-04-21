package algo.data.fenwick.immutable

import cats.kernel.CommutativeGroup

import scala.collection.immutable

private object GroupFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[CommutativeGroup, GroupFenwickTree]
    with FenwickTreeSparseFactoryDefaults[CommutativeGroup, GroupFenwickTree] {

  override protected def apply[V: CommutativeGroup](
      values: immutable.Vector[V]
  ): GroupFenwickTreeDense1D[V] =
    new GroupFenwickTreeDense1D(values, implicitly[CommutativeGroup[V]])

  override protected def apply[V: CommutativeGroup](
      size: Long
  ): GroupFenwickTree[Long, V] =
    new GroupFenwickTreeSparse1D[V](
      size,
      immutable.HashMap.empty,
      implicitly[CommutativeGroup[V]]
    )

  override protected def apply[V: CommutativeGroup](
      size1: Long,
      size2: Long
  ): GroupFenwickTree[(Long, Long), V] =
    new GroupFenwickTreeSparse2D(
      (size1, size2),
      immutable.HashMap.empty,
      implicitly[CommutativeGroup[V]]
    )

}
