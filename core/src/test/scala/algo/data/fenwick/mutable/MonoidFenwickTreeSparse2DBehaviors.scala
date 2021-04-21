package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.TestKit._

import scala.util.Random

trait MonoidFenwickTreeSparse2DBehaviors
    extends fenwick.MonoidFenwickTreeSparse2DBehaviors {

  def mutableSemigroup(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit = {
    combine(tabulate)
  }

  def combine(
      tabulate: ((Long, Long) => Long) => MonoidFenwickTree[(Long, Long), Long]
  ): Unit =
    "combine" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x)
      val (size1, size2) = tree.size

      // [
      //   [ 1, 2, 3, ... ]
      //   [ 1, 2, 3, ... ]
      //   [ 1, 2, 3, ... ]
      //   ...
      // ]
      for {
        x <- 0L until size1
        y <- 0L until size2
      } {
        tree.combine((x, y), 1)
      }

      for {
        x <- Iterable.fill(1_000)(Random.between(1L, size1 + 1))
        y <- Iterable.fill(1_000)(Random.between(1L, size2 + 1))
      } {
        val expectedSum = sumOfArithmeticProgression(1, x + 1) * y
        tree.reduceUntil((x, y)) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combined((-1, 0), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combined((size1, 0), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combined((0, -1), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combined((0, size2), 1)
      }

    }

}
