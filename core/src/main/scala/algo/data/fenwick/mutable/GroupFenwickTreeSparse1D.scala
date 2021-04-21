package algo.data.fenwick.mutable

import cats.kernel.CommutativeGroup

import scala.collection.mutable

private final class GroupFenwickTreeSparse1D[V](
    _size: Long,
    _values: mutable.HashMap[Long, V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[Long, V]
    with GroupFenwickTreeOps[Long, V, GroupFenwickTreeSparse1D[V]]
    with GroupFenwickTreeOpsDefaults[Long, V, GroupFenwickTreeSparse1D[V]]
    with MonoidFenwickTreeSparse1DOpsDefaults[V, GroupFenwickTreeSparse1D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: Long = _size

  override def clone(): GroupFenwickTreeSparse1D[V] =
    new GroupFenwickTreeSparse1D(size, _values.clone(), group)

  override protected def underlyingCollection: mutable.HashMap[Long, V] =
    _values

}
