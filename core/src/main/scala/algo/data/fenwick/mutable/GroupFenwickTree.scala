package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.{
  FenwickTreeFactory,
  FenwickTreeFactoryProxy,
  FenwickTreeSparseFactory,
  FenwickTreeSparseFactoryProxy
}
import cats.kernel.CommutativeGroup

trait GroupFenwickTree[K, V]
    extends fenwick.GroupFenwickTree[K, V]
    with MonoidFenwickTree[K, V]
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
