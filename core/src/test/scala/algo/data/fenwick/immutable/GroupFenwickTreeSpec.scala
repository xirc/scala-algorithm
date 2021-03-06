package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long.*

final class GroupFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.FenwickTreeSparseFactoryBehaviors
    with fenwick.GroupFenwickTreeBehaviors
    with fenwick.GroupFenwickTreeSparse2DBehaviors {

  import fenwick.FenwickTreeSparseFactorySyntax.*

  "immutable|GroupFenwickTree" should {

    behave like factory(GroupFenwickTree)
    behave like treeHasSize1D(GroupFenwickTree)
    behave like semigroup[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like monoid[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like group[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )

  }

  "immutable|GroupFenwickTree|Sparse1D" should {

    behave like factory1D(GroupFenwickTree)
    behave like treeHasSizeLarge1D(GroupFenwickTree)
    behave like semigroup[Long](
      GroupFenwickTree.tabulateS(size = 1_000, _)
    )
    behave like monoid[Long](
      GroupFenwickTree.tabulateS(size = 1_000, _)
    )
    behave like group[Long](
      GroupFenwickTree.tabulateS(size = 1_000, _)
    )

  }

  "immutable|GroupFenwickTree|Sparse2D" should {

    behave like factory2D(GroupFenwickTree)
    behave like treeHasSizeLarge2D(GroupFenwickTree)
    behave like super[GroupFenwickTreeSparse2DBehaviors].semigroup(
      GroupFenwickTree.tabulateS(size1 = 50, size2 = 20, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].monoid(
      GroupFenwickTree.tabulateS(size1 = 50, size2 = 20, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].group(
      GroupFenwickTree.tabulateS(size1 = 50, size2 = 20, _)
    )

  }

}
