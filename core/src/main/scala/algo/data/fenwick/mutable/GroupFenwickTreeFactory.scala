package algo.data.fenwick.mutable

import cats.kernel.CommutativeGroup

import scala.collection.mutable

private object GroupFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[CommutativeGroup, GroupFenwickTree]
    with FenwickTreeSparseFactoryDefaults[CommutativeGroup, GroupFenwickTree] {

  override protected def apply[V: CommutativeGroup](
      values: mutable.ArrayBuffer[V]
  ): GroupFenwickTree[Int, V] =
    new GroupFenwickTreeDense1D[V](values, implicitly[CommutativeGroup[V]])

  override protected def apply[V: CommutativeGroup](
      size: Long
  ): GroupFenwickTree[Long, V] =
    new GroupFenwickTreeSparse1D[V](
      size,
      mutable.HashMap.empty,
      implicitly[CommutativeGroup[V]]
    )

  override protected def apply[V: CommutativeGroup](
      size1: Long,
      size2: Long
  ): GroupFenwickTree[(Long, Long), V] =
    new GroupFenwickTreeSparse2D(
      (size1, size2),
      mutable.HashMap.empty,
      implicitly[CommutativeGroup[V]]
    )

}
