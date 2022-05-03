package algo.data.fenwick

import algo.testing.BaseSpec

final class MonoidFenwickTreeSpec
    extends BaseSpec
    with FenwickTreeFactoryBehaviors
    with FenwickTreeSparseFactoryBehaviors
    with MonoidFenwickTreeBehaviors
    with MonoidFenwickTreeSparse2DBehaviors {

  import FenwickTreeSparseFactorySyntax.*

  "MonoidFenwickTree|default" in {

    val instance = MonoidFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[immutable.MonoidFenwickTree[Int, ?]])

  }

  "MonoidFenwickTree|Sparse1D|default" in {

    val instance = MonoidFenwickTree[Long](size = 10)
    assert(instance.isInstanceOf[immutable.MonoidFenwickTree[Long, ?]])

  }

  "MonoidFenwickTree|Sparse2D|default" in {

    val instance = MonoidFenwickTree[Long](size1 = 10, size2 = 10)
    assert(instance.isInstanceOf[immutable.MonoidFenwickTree[(Long, Long), ?]])

  }

  "MonoidFenwickTree|mutable" in {

    val instance: MonoidFenwickTree[Int, Long] =
      mutable.MonoidFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[mutable.MonoidFenwickTree[?, ?]])

  }

  "MonoidFenwickTree|Sparse1D|mutable" in {

    val instance: MonoidFenwickTree[Long, Long] =
      mutable.MonoidFenwickTree[Long](size = 10)
    assert(instance.isInstanceOf[mutable.MonoidFenwickTree[?, ?]])

  }

  "MonoidFenwickTree|Sparse2D|mutable" in {

    val instance: MonoidFenwickTree[(Long, Long), Long] =
      mutable.MonoidFenwickTree[Long](size1 = 10, size2 = 10)
    assert(instance.isInstanceOf[mutable.MonoidFenwickTree[?, ?]])

  }

  "MonoidFenwickTree" should {

    behave like factory(MonoidFenwickTree)
    behave like treeHasSize1D(MonoidFenwickTree)
    behave like semigroup[Int](
      MonoidFenwickTree.tabulate(size = 100)
    )
    behave like monoid[Int](
      MonoidFenwickTree.tabulate(size = 100)
    )

  }

  "MonoidFenwickTree|Sparse1D" should {

    behave like factory1D(MonoidFenwickTree)
    behave like treeHasSizeLarge1D(MonoidFenwickTree)
    behave like semigroup[Long](
      MonoidFenwickTree.tabulateS(size = 100, _)
    )
    behave like monoid[Long](
      MonoidFenwickTree.tabulateS(size = 100, _)
    )

  }

  "MonoidFenwickTree|Sparse2D" should {

    behave like factory2D(MonoidFenwickTree)
    behave like treeHasSizeLarge2D(MonoidFenwickTree)
    behave like super[MonoidFenwickTreeSparse2DBehaviors].semigroup(
      MonoidFenwickTree.tabulateS(size1 = 10, size2 = 20, _)
    )
    behave like super[MonoidFenwickTreeSparse2DBehaviors].monoid(
      MonoidFenwickTree.tabulateS(size1 = 20, size2 = 10, _)
    )

  }

}
