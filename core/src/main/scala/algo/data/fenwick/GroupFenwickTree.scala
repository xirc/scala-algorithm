package algo.data.fenwick

import cats.kernel.CommutativeGroup

trait GroupFenwickTree[K, V]
    extends MonoidFenwickTree[K, V]
    with GroupFenwickTreeOps[K, V, GroupFenwickTree[K, V]]

object GroupFenwickTree
    extends FenwickTreeFactoryProxy[CommutativeGroup, GroupFenwickTree]
    with FenwickTreeSparseFactoryProxy[CommutativeGroup, GroupFenwickTree] {

  override protected def factory
      : FenwickTreeFactory[CommutativeGroup, GroupFenwickTree] =
    GroupFenwickTreeFactory

  override protected def sparseFactory
      : FenwickTreeSparseFactory[CommutativeGroup, GroupFenwickTree] =
    GroupFenwickTreeFactory

}
