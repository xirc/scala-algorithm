package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long._

final class MonoidFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.FenwickTreeSparseFactoryBehaviors
    with fenwick.MonoidFenwickTreeBehaviors
    with fenwick.MonoidFenwickTreeSparse2DBehaviors {

  import fenwick.FenwickTreeSparseFactorySyntax._

  "immutable|MonoidFenwickTree" should {

    behave like factory(MonoidFenwickTree)
    behave like treeHasSize1D(MonoidFenwickTree)
    behave like semigroup[Int](
      MonoidFenwickTree.tabulate(size = 100_000)
    )
    behave like monoid[Int](
      MonoidFenwickTree.tabulate(size = 100_000)
    )

  }

  "immutable|MonoidFenwickTree|Sparse1D" should {

    behave like factory1D(MonoidFenwickTree)
    behave like treeHasSizeLarge1D(MonoidFenwickTree)
    behave like semigroup[Long](
      MonoidFenwickTree.tabulateS(size = 100_000, _)
    )
    behave like monoid[Long](
      MonoidFenwickTree.tabulateS(size = 100_000, _)
    )

  }

  "immutable|MonoidFenwickTree|Sparse2D" should {

    behave like factory2D(MonoidFenwickTree)
    behave like treeHasSizeLarge2D(MonoidFenwickTree)
    behave like super[MonoidFenwickTreeSparse2DBehaviors].semigroup(
      MonoidFenwickTree.tabulateS(size1 = 500, size2 = 200, _)
    )
    behave like super[MonoidFenwickTreeSparse2DBehaviors].monoid(
      MonoidFenwickTree.tabulateS(size1 = 200, size2 = 500, _)
    )

  }

}
