package algo.data.fenwick.immutable

import cats.kernel.CommutativeMonoid

import scala.collection.immutable

private object MonoidFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[CommutativeMonoid, MonoidFenwickTree]
    with FenwickTreeSparseFactoryDefaults[
      CommutativeMonoid,
      MonoidFenwickTree
    ] {

  override protected def apply[V: CommutativeMonoid](
      values: immutable.Vector[V]
  ): MonoidFenwickTree[Int, V] =
    new MonoidFenwickTreeDense1D(values, implicitly[CommutativeMonoid[V]])

  override protected def apply[V: CommutativeMonoid](
      size: Long
  ): MonoidFenwickTree[Long, V] =
    new MonoidFenwickTreeSparse1D[V](
      size,
      immutable.HashMap.empty,
      implicitly[CommutativeMonoid[V]]
    )

  override protected def apply[V: CommutativeMonoid](
      size1: Long,
      size2: Long
  ): MonoidFenwickTree[(Long, Long), V] =
    new MonoidFenwickTreeSparse2D(
      (size1, size2),
      immutable.HashMap.empty,
      implicitly[CommutativeMonoid[V]]
    )

}
