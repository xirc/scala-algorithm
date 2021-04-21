package algo.data.fenwick.mutable

import algo.data.fenwick

import scala.util.Random

trait GroupFenwickTreeSparse2DBehaviors
    extends fenwick.GroupFenwickTreeSparse2DBehaviors
    with MonoidFenwickTreeSparse2DBehaviors {

  def mutableGroup(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit = {
    update(tabulate)
  }

  def update(
      tabulate: ((Long, Long) => Long) => GroupFenwickTree[(Long, Long), Long]
  ): Unit =
    "update" in {

      // [
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   [ 0, 1, 2, ... ]
      //   ...
      // ]
      val tree = tabulate((x, _) => x)
      val (size1, size2) = tree.size

      // [
      //   [ 1, 1, 1, ... ]
      //   [ 1, 1, 1, ... ]
      //   [ 1, 1, 1, ... ]
      //   ...
      // ]
      for {
        x <- 0L until size1
        y <- 0L until size2
      } {
        tree.update((x, y), 1)
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
        tree.foldRange((xl, yl), (xr, yr)) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated((-1, 0), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated((size1, 0), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated((0, -1), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated((0, size2), 1)
      }

    }

}
