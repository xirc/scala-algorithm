package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long._

final class GroupFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeBehaviors
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.FenwickTreeSparseFactoryBehaviors
    with GroupFenwickTreeBehaviors
    with GroupFenwickTreeSparse2DBehaviors {

  import fenwick.FenwickTreeSparseFactorySyntax._

  "mutable|GroupFenwickTree" should {

    behave like factory(GroupFenwickTree)
    behave like treeHasSize1D(GroupFenwickTree)
    behave like semigroup[Int](
      GroupFenwickTree.tabulate(size = 100_000)
    )
    behave like mutableSemigroup[Int](
      GroupFenwickTree.tabulate(size = 100_000)
    )
    behave like monoid[Int](
      GroupFenwickTree.tabulate(size = 100_000)
    )
    behave like group[Int](
      GroupFenwickTree.tabulate(size = 100_000)
    )
    behave like mutableGroup[Int](
      GroupFenwickTree.tabulate(size = 100_000)
    )

  }

  "mutable|GroupFenwickTree|Sparse1D" should {

    behave like factory1D(GroupFenwickTree)
    behave like treeHasSizeLarge1D(GroupFenwickTree)
    behave like semigroup[Long](
      GroupFenwickTree.tabulateS(size = 10_000, _)
    )
    behave like monoid[Long](
      GroupFenwickTree.tabulateS(size = 100_000, _)
    )
    behave like group[Long](
      GroupFenwickTree.tabulateS(size = 10_000, _)
    )

  }

  "mutable|GroupFenwickTree|Sparse2D" should {

    behave like factory1D(GroupFenwickTree)
    behave like treeHasSizeLarge2D(GroupFenwickTree)
    behave like super[GroupFenwickTreeSparse2DBehaviors].semigroup(
      GroupFenwickTree.tabulateS(200, 100, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].mutableSemigroup(
      GroupFenwickTree.tabulateS(200, 500, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].monoid(
      GroupFenwickTree.tabulateS(500, 200, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].group(
      GroupFenwickTree.tabulateS(100, 200, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].mutableGroup(
      GroupFenwickTree.tabulateS(200, 500, _)
    )

  }

}
