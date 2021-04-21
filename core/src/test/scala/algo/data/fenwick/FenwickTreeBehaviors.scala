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

      factory.tabulate(0)(identity).size shouldBe 0
      factory.tabulate(1)(identity).size shouldBe 1
      factory.tabulate(2)(identity).size shouldBe 2
      factory.tabulate(3)(identity).size shouldBe 3
      factory.tabulate(100)(identity).size shouldBe 100
      factory.tabulate(1_000)(identity).size shouldBe 1_000
      factory.tabulate(10_000)(identity).size shouldBe 10_000
      factory.tabulate(100_000)(identity).size shouldBe 100_000

    }

  def treeHasSizeLarge1D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit =
    "size" in {

      factory[Long](0).size shouldBe 0
      factory[Long](1).size shouldBe 1
      factory[Long](2).size shouldBe 2
      factory[Long](3).size shouldBe 3
      factory[Long](100).size shouldBe 100
      factory[Long](1_000).size shouldBe 1_000
      factory[Long](10_000).size shouldBe 10_000
      factory[Long](100_000).size shouldBe 100_000
      factory[Long](1_000_000).size shouldBe 1_000_000
      factory[Long](10_000_000).size shouldBe 10_000_000
      factory[Long](100_000_000).size shouldBe 100_000_000
      factory[Long](1000_000_000L).size shouldBe 1000_000_000L
      factory[Long](10_000_000_000L).size shouldBe 10_000_000_000L
      factory[Long](100_000_000_000L).size shouldBe 100_000_000_000L
      factory[Long](1000_000_000_000L).size shouldBe 1000_000_000_000L

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
        factory[Long](size1, size2).size shouldBe expectedSize
      }

    }

}
