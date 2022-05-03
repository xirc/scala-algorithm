package algo.data.fenwick

import algo.data.fenwick.TestKit.*
import algo.testing.BaseSpec

import scala.util.Random

trait GroupFenwickTreeSparse2DBehaviors
    extends BaseSpec
    with MonoidFenwickTreeSparse2DBehaviors {

  def group(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit = {
    foldRange(tabulate)
    apply(tabulate)
    updated(tabulate)
  }

  def foldRange(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit =
    "foldRange" in {

      // [
      //   [ 1, 2, 3, 4, ... ],
      //   [ 1, 2, 3, 4, ... ],
      //   ...
      // ]
      val tree = tabulate((x, _) => x + 1)
      val (size1, size2) = tree.size

      def sum(from: (Long, Long), until: (Long, Long)): Long = {
        val (xl, yl) = from
        val (xr, yr) = until
        if (xl >= xr || yl >= yr) {
          0
        } else {
          sumOfArithmeticProgression(0, xr + 1) * yr -
            sumOfArithmeticProgression(0, xr + 1) * yl -
            sumOfArithmeticProgression(0, xl + 1) * yr +
            sumOfArithmeticProgression(0, xl + 1) * yl
        }
      }

      for {
        xl <- Iterable.fill(50)(Random.between(0L, size1))
        xr <- Iterable.fill(20)(Random.between(0L, size1 + 1))
        yl <- Iterable.fill(50)(Random.between(0L, size2))
        yr <- Iterable.fill(20)(Random.between(0L, size2 + 1))
      } yield {
        val expectedSum = sum((xl, yl), (xr, yr))
        assert(tree.foldRange((xl, yl), (xr, yr)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((-1, 1), (0, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((size1 + 1, 1), (0, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((1, -1), (0, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((1, size2 + 1), (0, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((0, 0), (-1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((0, 0), (size1 + 1, 1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((0, 0), (1, -1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange((0, 0), (1, size2 + 1))
      }

    }

  def apply(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit =
    "apply" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 1, 2, 3, ... ]
      //   [ 2, 3, 4, ... ]
      //   ...
      // ]
      val tree = tabulate((x, y) => x + y)
      val (size1, size2) = tree.size

      for {
        x <- Iterable.fill(500)(Random.between(0L, size1))
        y <- Iterable.fill(500)(Random.between(0L, size2))
      } yield {
        assert(tree((x, y)) === (x + y))
      }

      intercept[IndexOutOfBoundsException] {
        tree((-1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree((size1, 0))
      }

      intercept[IndexOutOfBoundsException] {
        tree((0, -1))
      }

      intercept[IndexOutOfBoundsException] {
        tree((0, size2))
      }

    }

  def updated(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit =
    "updated" in {

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
        tree = tree.updated((x, y), 1)
      }

      def sum(lower: (Long, Long), upper: (Long, Long)): Long = {
        val (xl, yl) = lower
        val (xr, yr) = upper
        if (xl >= xr || yl >= yr) {
          0
        } else {
          xr * yr - xr * yl - xl * yr + xl * yl
        }
      }

      for {
        xl <- Iterable.fill(50)(Random.between(0L, size1))
        xr <- Iterable.fill(20)(Random.between(0L, size1 + 1))
        yl <- Iterable.fill(50)(Random.between(0L, size2))
        yr <- Iterable.fill(20)(Random.between(0L, size2 + 1))
      } yield {
        val expectedSum = sum((xl, yl), (xr, yr))
        assert(tree.foldRange((xl, yl), (xr, yr)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated((-1, 0), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated((size1, 0), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated((0, -1), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated((0, size2), 1)
      }

    }

}
