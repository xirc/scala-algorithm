package algo.data.fenwick.mutable

import cats.kernel.CommutativeMonoid

import scala.collection.mutable

private object MonoidFenwickTreeFactory
    extends FenwickTreeFactoryDefaults[
      CommutativeMonoid,
      MonoidFenwickTree
    ]
    with FenwickTreeSparseFactoryDefaults[
      CommutativeMonoid,
      MonoidFenwickTree
    ] {

  override protected def apply[V: CommutativeMonoid](
      values: mutable.ArrayBuffer[V]
  ): MonoidFenwickTree[Int, V] =
    new MonoidFenwickTreeDense1D(values, implicitly[CommutativeMonoid[V]])

  override protected def apply[V: CommutativeMonoid](
      size: Long
  ): MonoidFenwickTree[Long, V] =
    new MonoidFenwickTreeSparse1D[V](
      size,
      mutable.HashMap.empty,
      implicitly[CommutativeMonoid[V]]
    )

  override protected def apply[V: CommutativeMonoid](
      size1: Long,
      size2: Long
  ): MonoidFenwickTree[(Long, Long), V] =
    new MonoidFenwickTreeSparse2D[V](
      (size1, size2),
      mutable.HashMap.empty,
      implicitly[CommutativeMonoid[V]]
    )

}
