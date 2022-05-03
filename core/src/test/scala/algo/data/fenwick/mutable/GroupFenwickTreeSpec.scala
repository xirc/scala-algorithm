package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long.*

final class GroupFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeBehaviors
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.FenwickTreeSparseFactoryBehaviors
    with GroupFenwickTreeBehaviors
    with GroupFenwickTreeSparse2DBehaviors {

  import fenwick.FenwickTreeSparseFactorySyntax.*

  "mutable|GroupFenwickTree" should {

    behave like factory(GroupFenwickTree)
    behave like treeHasSize1D(GroupFenwickTree)
    behave like semigroup[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like mutableSemigroup[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like monoid[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like group[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )
    behave like mutableGroup[Int](
      GroupFenwickTree.tabulate(size = 1_000)
    )

  }

  "mutable|GroupFenwickTree|Sparse1D" should {

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

  "mutable|GroupFenwickTree|Sparse2D" should {

    behave like factory1D(GroupFenwickTree)
    behave like treeHasSizeLarge2D(GroupFenwickTree)
    behave like super[GroupFenwickTreeSparse2DBehaviors].semigroup(
      GroupFenwickTree.tabulateS(20, 10, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].mutableSemigroup(
      GroupFenwickTree.tabulateS(20, 50, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].monoid(
      GroupFenwickTree.tabulateS(50, 20, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].group(
      GroupFenwickTree.tabulateS(10, 20, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].mutableGroup(
      GroupFenwickTree.tabulateS(20, 50, _)
    )

  }

}
