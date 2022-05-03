package algo.data.fenwick

import algo.data.fenwick.TestKit.*
import algo.testing.BaseSpec
import cats.kernel.CommutativeGroup

trait FenwickTreeSparseFactoryBehaviors extends BaseSpec {

  def factory1D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = {
    from1D(factory)
    apply1D(factory)
  }

  def from1D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.from|1D" in {

    // [ 0 -> 1, 1 -> 2, 2 -> 3, ..., ]
    val size = 10_000L
    val values = (0L until size).map(i => i -> (i + 1))

    val tree = factory.from(size, values)
    for (x <- 1L to size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      assert(tree.reduceUntil(x) === expectedSum)
    }

    assert(factory.from[Long](0, Iterator.empty).size === 0L)

    intercept[IllegalArgumentException] {
      factory.from[Long](-1, Iterator.empty)
    }

  }

  def apply1D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.apply|1D" in {

    // [ 0 -> 1, 1 -> 2, 2 -> 3, ..., ]
    val size = 10_000L
    val values = (0L until size).map(i => i -> (i + 1))

    val tree = factory(size, values*)
    for (x <- 1L to size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      assert(tree.reduceUntil(x) === expectedSum)
    }

    assert(factory[Long](0).size === 0L)

    intercept[IllegalArgumentException] {
      factory[Long](-1)
    }

  }

  def factory2D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = {
    from2D(factory)
    apply2D(factory)
  }

  def from2D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.from|2D" in {

    val size1 = 100L
    val size2 = 200L
    val values = for {
      x <- 0L until size1
      y <- 0L until size2
    } yield {
      (x, y) -> (x + 1)
    }

    val tree = factory.from(size1, size2, values)
    for (x <- 1L to size1) {
      for (y <- 1L to size2) {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        assert(tree.reduceUntil((x, y)) === expectedSum)
      }
    }

    assert(factory.from[Long](0, 0, Iterator.empty).size === (0L -> 0L))

    intercept[IllegalArgumentException] {
      factory.from[Long](-1, 0, Iterator.empty)
    }

    intercept[IllegalArgumentException] {
      factory.from[Long](0, -1, Iterator.empty)
    }

  }

  def apply2D(
      factory: FenwickTreeSparseFactory[
        CommutativeGroup,
        MonoidFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.apply|2D" in {

    val size1 = 10L
    val size2 = 20L
    val values = for {
      x <- 0L until size1
      y <- 0L until size2
    } yield {
      (x, y) -> (x + 1)
    }

    val tree = factory(size1, size2, values*)
    for (x <- 1L to size1) {
      for (y <- 1L to size2) {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        assert(tree.reduceUntil((x, y)) === expectedSum)
      }
    }

    intercept[IllegalArgumentException] {
      factory[Long](-1, 0)
    }

    intercept[IllegalArgumentException] {
      factory[Long](0, -1)
    }

  }

}
