package algo.data.fenwick

import cats.kernel.CommutativeGroup

private object GroupFenwickTreeFactory
    extends FenwickTreeFactory[CommutativeGroup, GroupFenwickTree]
    with FenwickTreeSparseFactory[
      CommutativeGroup,
      GroupFenwickTree
    ] {

  override def from[V: CommutativeGroup](
      iterable: IterableOnce[V]
  ): GroupFenwickTree[Int, V] =
    immutable.GroupFenwickTree.from(iterable)

  override def from[V: CommutativeGroup](
      size: Long,
      iterable: IterableOnce[(Long, V)]
  ): GroupFenwickTree[Long, V] =
    immutable.GroupFenwickTree.from(size, iterable)

  override def from[V: CommutativeGroup](
      size1: Long,
      size2: Long,
      iterable: IterableOnce[((Long, Long), V)]
  ): GroupFenwickTree[(Long, Long), V] =
    immutable.GroupFenwickTree.from(size1, size2, iterable)

}
