package algo.data.fenwick

import algo.testing.BaseSpec

final class GroupFenwickTreeSpec
    extends BaseSpec
    with FenwickTreeFactoryBehaviors
    with FenwickTreeSparseFactoryBehaviors
    with GroupFenwickTreeBehaviors
    with GroupFenwickTreeSparse2DBehaviors {

  import FenwickTreeSparseFactorySyntax.*

  "GroupFenwickTree|default" in {

    val instance = GroupFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[immutable.GroupFenwickTree[Int, ?]])

  }

  "GroupFenwickTree|Sparse1D|default" in {

    val instance = GroupFenwickTree[Long](size = 10)
    assert(instance.isInstanceOf[immutable.GroupFenwickTree[Long, ?]])

  }

  "GroupFenwickTree|Sparse2D|default" in {

    val instance = GroupFenwickTree[Long](size1 = 10, size2 = 10)
    assert(instance.isInstanceOf[immutable.GroupFenwickTree[(Long, Long), ?]])

  }

  "GroupFenwickTree|mutable" in {

    val instance: GroupFenwickTree[Int, Long] =
      mutable.GroupFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[mutable.GroupFenwickTree[?, ?]])

  }

  "GroupFenwickTree|Sparse1D|mutable" in {

    val instance: GroupFenwickTree[Long, Long] =
      mutable.GroupFenwickTree[Long](size = 10)
    assert(instance.isInstanceOf[mutable.GroupFenwickTree[?, ?]])

  }

  "GroupFenwickTree|Sparse2D|mutable" in {

    val instance: GroupFenwickTree[(Long, Long), Long] =
      mutable.GroupFenwickTree[Long](size1 = 10, size2 = 10)
    assert(instance.isInstanceOf[mutable.GroupFenwickTree[?, ?]])

  }

  "GroupFenwickTree" should {

    behave like factory(GroupFenwickTree)
    behave like treeHasSize1D(GroupFenwickTree)
    behave like semigroup[Int](
      GroupFenwickTree.tabulate(size = 100)
    )
    behave like monoid[Int](
      GroupFenwickTree.tabulate(size = 100)
    )
    behave like group[Int](
      GroupFenwickTree.tabulate(size = 100)
    )

  }

  "GroupFenwickTree|Sparse1D" should {

    behave like factory1D(GroupFenwickTree)
    behave like treeHasSizeLarge1D(GroupFenwickTree)
    behave like semigroup[Long](
      GroupFenwickTree.tabulateS(size = 100, _)
    )
    behave like monoid[Long](
      GroupFenwickTree.tabulateS(size = 100, _)
    )

  }

  "GroupFenwickTree|Sparse2D" should {

    behave like factory2D(GroupFenwickTree)
    behave like treeHasSizeLarge2D(GroupFenwickTree)
    behave like super[GroupFenwickTreeSparse2DBehaviors].semigroup(
      GroupFenwickTree.tabulateS(size1 = 10, size2 = 20, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].monoid(
      GroupFenwickTree.tabulateS(size1 = 20, size2 = 10, _)
    )

  }

}
