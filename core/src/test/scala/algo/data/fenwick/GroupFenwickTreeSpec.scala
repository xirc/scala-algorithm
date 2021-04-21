package algo.data.fenwick

import algo.testing.BaseSpec

final class GroupFenwickTreeSpec
    extends BaseSpec
    with FenwickTreeFactoryBehaviors
    with FenwickTreeSparseFactoryBehaviors
    with GroupFenwickTreeBehaviors
    with GroupFenwickTreeSparse2DBehaviors {

  import FenwickTreeSparseFactorySyntax._

  "GroupFenwickTree|default" in {

    val instance = GroupFenwickTree.tabulate(size = 10)(_.toLong)
    instance shouldBe a[immutable.GroupFenwickTree[Int, _]]

  }

  "GroupFenwickTree|Sparse1D|default" in {

    val instance = GroupFenwickTree[Long](size = 10)
    instance shouldBe a[immutable.GroupFenwickTree[Long, _]]

  }

  "GroupFenwickTree|Sparse2D|default" in {

    val instance = GroupFenwickTree[Long](size1 = 10, size2 = 10)
    instance shouldBe a[immutable.GroupFenwickTree[(Long, Long), _]]

  }

  "GroupFenwickTree|mutable" in {

    val instance: GroupFenwickTree[Int, Long] =
      mutable.GroupFenwickTree.tabulate(size = 10)(_.toLong)
    instance shouldBe a[mutable.GroupFenwickTree[_, _]]

  }

  "GroupFenwickTree|Sparse1D|mutable" in {

    val instance: GroupFenwickTree[Long, Long] =
      mutable.GroupFenwickTree[Long](size = 10)
    instance shouldBe a[mutable.GroupFenwickTree[_, _]]

  }

  "GroupFenwickTree|Sparse2D|mutable" in {

    val instance: GroupFenwickTree[(Long, Long), Long] =
      mutable.GroupFenwickTree[Long](size1 = 10, size2 = 10)
    instance shouldBe a[mutable.GroupFenwickTree[_, _]]

  }

  "GroupFenwickTree" should {

    behave like factory(MonoidFenwickTree)
    behave like treeHasSize1D(MonoidFenwickTree)
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
      GroupFenwickTree.tabulateS(size1 = 100, size2 = 200, _)
    )
    behave like super[GroupFenwickTreeSparse2DBehaviors].monoid(
      GroupFenwickTree.tabulateS(size1 = 200, size2 = 100, _)
    )

  }

}
