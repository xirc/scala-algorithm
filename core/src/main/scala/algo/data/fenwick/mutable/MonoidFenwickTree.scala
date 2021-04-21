package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.{
  FenwickTreeFactory,
  FenwickTreeFactoryProxy,
  FenwickTreeSparseFactory,
  FenwickTreeSparseFactoryProxy
}
import cats.kernel.CommutativeMonoid

trait MonoidFenwickTree[K, V]
    extends fenwick.MonoidFenwickTree[K, V]
    with SemigroupFenwickTree[K, V]
    with MonoidFenwickTreeOps[K, V, MonoidFenwickTree[K, V]]

object MonoidFenwickTree
    extends FenwickTreeFactoryProxy[CommutativeMonoid, MonoidFenwickTree]
    with FenwickTreeSparseFactoryProxy[CommutativeMonoid, MonoidFenwickTree] {

  override protected def factory
      : FenwickTreeFactory[CommutativeMonoid, MonoidFenwickTree] =
    MonoidFenwickTreeFactory

  override protected def sparseFactory
      : FenwickTreeSparseFactory[CommutativeMonoid, MonoidFenwickTree] =
    MonoidFenwickTreeFactory

}
