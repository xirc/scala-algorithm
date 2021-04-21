package algo.data.fenwick

import cats.kernel.CommutativeMonoid

private object MonoidFenwickTreeFactory
    extends FenwickTreeFactory[CommutativeMonoid, MonoidFenwickTree]
    with FenwickTreeSparseFactory[
      CommutativeMonoid,
      MonoidFenwickTree
    ] {

  override def from[V: CommutativeMonoid](
      iterable: IterableOnce[V]
  ): MonoidFenwickTree[Int, V] =
    immutable.MonoidFenwickTree.from(iterable)

  override def from[V: CommutativeMonoid](
      size: Long,
      iterable: IterableOnce[(Long, V)]
  ): MonoidFenwickTree[Long, V] =
    immutable.MonoidFenwickTree.from(size, iterable)

  override def from[V: CommutativeMonoid](
      size1: Long,
      size2: Long,
      iterable: IterableOnce[((Long, Long), V)]
  ): MonoidFenwickTree[(Long, Long), V] =
    immutable.MonoidFenwickTree.from(size1, size2, iterable)

}
