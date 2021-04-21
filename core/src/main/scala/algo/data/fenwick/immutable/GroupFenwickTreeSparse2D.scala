package algo.data.fenwick.immutable

import cats.kernel.CommutativeGroup

import scala.collection.immutable
import scala.collection.immutable.HashMap

private final class GroupFenwickTreeSparse2D[V](
    _size: (Long, Long),
    _values: immutable.HashMap[(Long, Long), V],
    ev: CommutativeGroup[V]
) extends GroupFenwickTree[(Long, Long), V]
    with GroupFenwickTreeOps[(Long, Long), V, GroupFenwickTreeSparse2D[V]]
    with GroupFenwickTreeOpsDefaults[(Long, Long), V, GroupFenwickTreeSparse2D[
      V
    ]]
    with MonoidFenwickTreeSparse2DOpsDefaults[V, GroupFenwickTreeSparse2D[V]] {

  override implicit def group: CommutativeGroup[V] = ev

  override def size: (Long, Long) = _size

  override protected def build(
      values: HashMap[(Long, Long), V]
  ): GroupFenwickTreeSparse2D[V] =
    new GroupFenwickTreeSparse2D(size, values, group)

  override protected def underlyingCollection: HashMap[(Long, Long), V] =
    _values

}
