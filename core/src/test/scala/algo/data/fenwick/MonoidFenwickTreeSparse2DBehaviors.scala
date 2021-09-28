package algo.data.fenwick

import algo.data.fenwick.TestKit._
import algo.testing.BaseSpec

import scala.util.Random

trait MonoidFenwickTreeSparse2DBehaviors extends BaseSpec {

  def semigroup(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit = {
    reduceUntil(tabulate)
    reduceTo(tabulate)
    combined(tabulate)
  }

  def monoid(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit = {
    foldUntil(tabulate)
    foldTo(tabulate)
  }

  def reduceUntil(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "reduceUntil" in {

      // [
      //   [ 1, 2, 3, ... ]
      //   [ 1, 2, 3, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x + 1)
      val (size1, size2) = tree.size

      for {
        x <- Iterable.fill(1000)(Random.between(1L, size1 + 1))
        y <- Iterable.fill(1000)(Random.between(1L, size2 + 1))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        assert(tree.reduceUntil((x, y)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((0, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((-1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((size1 + 1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((1, -1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil((1, size2 + 1))
      }

    }

  def reduceTo(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "reduceTo" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x)
      val (size1, size2) = tree.size

      for {
        x <- Iterable.fill(1_000)(Random.between(0L, size1))
        y <- Iterable.fill(1_000)(Random.between(0L, size2))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * (y + 1)
        assert(tree.reduceTo((x, y)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((-1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((size1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((-1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((0, size2))
      }

    }

  def combined(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "combined" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   ...
      // ]
      var tree = tabulate((x, _) => x)
      val (size1, size2) = tree.size

      for {
        x <- 0L until size1
        y <- 0L until size2
      } {
        tree = tree.combined((x, y), 1)
      }

      for {
        x <- Iterable.fill(1_000)(Random.between(1L, size1 + 1))
        y <- Iterable.fill(1_000)(Random.between(1L, size2 + 1))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        assert(tree.reduceUntil((x, y)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined((-1, 0), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined((size1, 0), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined((0, -1), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined((0, size2), 1)
      }

    }

  def foldUntil(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "foldUntil" in {

      // [
      //   [ 1, 2, 3, ... ]
      //   [ 1, 2, 3, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x + 1)
      val (size1, size2) = tree.size

      for {
        x <- Iterable.fill(1_000)(Random.between(0L, size1 + 1))
        y <- Iterable.fill(1_000)(Random.between(0L, size2 + 1))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        assert(tree.foldUntil((x, y)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldUntil((-1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldUntil((size1 + 1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldUntil((1, -1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldUntil((1, size2 + 1))
      }

    }

  def foldTo(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "foldTo" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x)
      val (size1, size2) = tree.size

      for {
        x <- Iterable.fill(1_000)(Random.between(0L, size1))
        y <- Iterable.fill(1_000)(Random.between(0L, size2))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * (y + 1)
        assert(tree.foldTo((x, y)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((-1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((size1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((-1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo((0, size2))
      }

    }

}
