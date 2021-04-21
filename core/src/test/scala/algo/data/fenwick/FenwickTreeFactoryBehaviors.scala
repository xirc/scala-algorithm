package algo.data.fenwick

import algo.data.fenwick.TestKit._
import algo.testing.BaseSpec
import cats.kernel.CommutativeGroup

trait FenwickTreeFactoryBehaviors extends BaseSpec {

  def factory(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = {
    from(factory)
    apply(factory)
    iterate(factory)
    unfold(factory)
    range2(factory)
    range3(factory)
    concat(factory)
    fill(factory)
    tabulate(factory)
  }

  def from(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit =
    s"${factory.getClass.getSimpleName}.from" in {

      val tree = factory.from(Array(1, 2, 3, 4, 5))
      tree.size shouldBe 5
      for (x <- 1 to tree.size) {
        val expectedSum = sumOfArithmeticProgression(1, x + 1)
        tree.reduceUntil(x) shouldBe expectedSum
      }

    }

  def apply(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.apply" in {

    val tree = factory(1, 2, 3, 4, 5)
    tree.size shouldBe 5
    for (x <- 1 to tree.size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def iterate(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.iterate" in {

    // [ 1, 2, 3, ... , 9, 10 ]
    val tree = factory.iterate(1, 10)(_ + 1)
    tree.size shouldBe 10
    for (x <- 1 to tree.size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def unfold(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.unfold" in {

    // [ 1, 2, 3, ..., 9, 10 ]
    val tree =
      factory.unfold(1) { s =>
        Option(s).filter(_ <= 10).map(s => (s, s + 1))
      }
    tree.size shouldBe 10
    for (x <- 1 to tree.size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def range2(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.range2" in {

    // [ 100, 101, 102, ..., 198, 199 ]
    val tree = factory.range(100, 200)
    val values = (100 until 200).toVector
    tree.size shouldBe values.size
    for (x <- 1 to tree.size) {
      val expectedSum = values.view.take(x).sum
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def range3(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.range3" in {

    // [ 100, 103, 106, ..., 193, 196, 199 ]
    val tree = factory.range(100, 200, 3)
    val values = (100 until 200 by 3).toVector
    tree.size shouldBe values.size
    for (x <- 1 to tree.size) {
      val expectedSum = values.view.take(x).sum
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def concat(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.concat" in {

    // [ 1, 2, 3, ..., 99, 100 ]
    val iterable1 = Vector.tabulate(100)(_ + 1)
    // [ 101, 102, 103, ..., 199, 200 ]
    val iterable2 = Vector.tabulate(100)(_ + 100 + 1)
    val tree = factory.concat(iterable1, iterable2)
    val expectedSize = (iterable1.size + iterable2.size)
    tree.size shouldBe expectedSize
    for (x <- 1 to tree.size) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

  def fill(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit =
    s"${factory.getClass.getSimpleName}.fill" in {

      // [2, 2, 2, ... ]
      val filledValue = 2
      val tree = factory.fill(100)(filledValue)
      for (x <- 1 to 100) {
        val expectedSum = filledValue * x
        tree.reduceUntil(x) shouldBe expectedSum
      }

    }

  def tabulate(
      factory: FenwickTreeFactory[
        CommutativeGroup,
        SemigroupFenwickTree
      ]
  ): Unit = s"${factory.getClass.getSimpleName}.tabulate" in {

    // [ 1, 2, 3, ..., 98, 99, 100]
    val tree = factory.tabulate(100)(_ + 1)
    for (x <- 1 to 100) {
      val expectedSum = sumOfArithmeticProgression(1, x + 1)
      tree.reduceUntil(x) shouldBe expectedSum
    }

  }

}
