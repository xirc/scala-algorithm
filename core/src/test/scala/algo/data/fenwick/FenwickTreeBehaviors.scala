package algo.data.fenwick

import algo.testing.BaseSpec
import cats.kernel.CommutativeGroup

import scala.util.Random

trait FenwickTreeBehaviors extends BaseSpec {

  def treeHasSize1D(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit =
    "size" in {

      assert(factory.tabulate(0)(identity).size === 0)
      assert(factory.tabulate(1)(identity).size === 1)
      assert(factory.tabulate(2)(identity).size === 2)
      assert(factory.tabulate(3)(identity).size === 3)
      assert(factory.tabulate(100)(identity).size === 100)
      assert(factory.tabulate(1_000)(identity).size === 1_000)
      assert(factory.tabulate(10_000)(identity).size === 10_000)
      assert(factory.tabulate(100_000)(identity).size === 100_000)

    }

  def treeHasSizeLarge1D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit =
    "size" in {

      assert(factory[Long](0).size === 0L)
      assert(factory[Long](1).size === 1L)
      assert(factory[Long](2).size === 2L)
      assert(factory[Long](3).size === 3L)
      assert(factory[Long](100).size === 100L)
      assert(factory[Long](1_000).size === 1_000L)
      assert(factory[Long](10_000).size === 10_000L)
      assert(factory[Long](100_000).size === 100_000L)
      assert(factory[Long](1_000_000).size === 1_000_000L)
      assert(factory[Long](10_000_000).size === 10_000_000L)
      assert(factory[Long](100_000_000).size === 100_000_000L)
      assert(factory[Long](1000_000_000L).size === 1000_000_000L)
      assert(factory[Long](10_000_000_000L).size === 10_000_000_000L)
      assert(factory[Long](100_000_000_000L).size === 100_000_000_000L)
      assert(factory[Long](1000_000_000_000L).size === 1000_000_000_000L)

    }

  def treeHasSizeLarge2D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit =
    "size" in {

      for {
        size1 <- Iterator.fill(1000)(Random.between(0, 1000_000_000_000L))
        size2 <- Iterator.fill(1000)(Random.between(0, 1000_000_000_000L))
      } {
        val expectedSize = (size1, size2)
        assert(factory[Long](size1, size2).size === expectedSize)
      }

    }

}
