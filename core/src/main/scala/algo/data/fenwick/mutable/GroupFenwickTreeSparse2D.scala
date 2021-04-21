package algo.data.fenwick.mutable

import cats.kernel.CommutativeGroup

import scala.collection.mutable

private final class GroupFenwickTreeSparse2D[V](
    _size: (Long, Long),
    _values: mutable.HashMap[(Long, Long), V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[(Long, Long), V]
    with GroupFenwickTreeOps[(Long, Long), V, GroupFenwickTreeSparse2D[V]]
    with GroupFenwickTreeOpsDefaults[(Long, Long), V, GroupFenwickTreeSparse2D[
      V
    ]]
    with MonoidFenwickTreeSparse2DOpsDefaults[V, GroupFenwickTreeSparse2D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: (Long, Long) = _size

  override protected def underlyingCollection
      : mutable.HashMap[(Long, Long), V] = _values

  override def clone(): GroupFenwickTreeSparse2D[V] =
    new GroupFenwickTreeSparse2D(size, _values.clone(), group)

}
