package algo.data.fenwick

import cats.kernel.CommutativeGroup

package object immutable {

  object FenwickTree
      extends FenwickTreeFactoryProxy[
        CommutativeGroup,
        GroupFenwickTree
      ]
      with FenwickTreeSparseFactoryProxy[CommutativeGroup, GroupFenwickTree] {

    override protected def factory
        : FenwickTreeFactory[CommutativeGroup, GroupFenwickTree] =
      GroupFenwickTreeFactory

    override protected def sparseFactory
        : FenwickTreeSparseFactory[CommutativeGroup, GroupFenwickTree] =
      GroupFenwickTreeFactory

  }

}
