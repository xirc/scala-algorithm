package algo.data.fenwick

import cats.kernel.CommutativeMonoid

trait MonoidFenwickTree[K, V]
    extends SemigroupFenwickTree[K, V]
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
