package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long.*

final class MonoidFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.FenwickTreeSparseFactoryBehaviors
    with fenwick.MonoidFenwickTreeBehaviors
    with fenwick.MonoidFenwickTreeSparse2DBehaviors {

  import fenwick.FenwickTreeSparseFactorySyntax.*

  "immutable|MonoidFenwickTree" should {

    behave like factory(MonoidFenwickTree)
    behave like treeHasSize1D(MonoidFenwickTree)
    behave like semigroup[Int](
      MonoidFenwickTree.tabulate(size = 1_000)
    )
    behave like monoid[Int](
      MonoidFenwickTree.tabulate(size = 1_000)
    )

  }

  "immutable|MonoidFenwickTree|Sparse1D" should {

    behave like factory1D(MonoidFenwickTree)
    behave like treeHasSizeLarge1D(MonoidFenwickTree)
    behave like semigroup[Long](
      MonoidFenwickTree.tabulateS(size = 1_000, _)
    )
    behave like monoid[Long](
      MonoidFenwickTree.tabulateS(size = 1_000, _)
    )

  }

  "immutable|MonoidFenwickTree|Sparse2D" should {

    behave like factory2D(MonoidFenwickTree)
    behave like treeHasSizeLarge2D(MonoidFenwickTree)
    behave like super[MonoidFenwickTreeSparse2DBehaviors].semigroup(
      MonoidFenwickTree.tabulateS(size1 = 50, size2 = 20, _)
    )
    behave like super[MonoidFenwickTreeSparse2DBehaviors].monoid(
      MonoidFenwickTree.tabulateS(size1 = 20, size2 = 50, _)
    )

  }

}
