package algo.data.fenwick.immutable

import cats.kernel.CommutativeGroup

import scala.collection.immutable

private final class GroupFenwickTreeSparse1D[V](
    _size: Long,
    _values: immutable.HashMap[Long, V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[Long, V]
    with GroupFenwickTreeOps[Long, V, GroupFenwickTreeSparse1D[V]]
    with MonoidFenwickTreeSparse1DOpsDefaults[V, GroupFenwickTreeSparse1D[V]]
    with GroupFenwickTreeOpsDefaults[Long, V, GroupFenwickTreeSparse1D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: Long = _size

  override protected def build(
      values: immutable.HashMap[Long, V]
  ): GroupFenwickTreeSparse1D[V] =
    new GroupFenwickTreeSparse1D(size, values, group)

  override protected def underlyingCollection: immutable.HashMap[Long, V] =
    _values

}
