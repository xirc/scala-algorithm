package algo.data.fenwick

import algo.data.fenwick.TestKit._
import algo.testing.BaseSpec

import scala.Integral.Implicits._
import scala.util.Random

trait GroupFenwickTreeBehaviors
    extends BaseSpec
    with MonoidFenwickTreeBehaviors {

  def group[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit = {
    foldRange(tabulate)
    apply(tabulate)
    updated(tabulate)
    asMonoidFenwickTree(tabulate(_.toLong))
  }

  def foldRange[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit =
    "foldRange" in {

      // [0, 1, 2, ...]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for {
        l <- Iterable.fill(1000)(Random.between(0, size.toInt))
        r <- Iterable.fill(1000)(Random.between(0, size.toInt))
      } {
        // sum of [0, 1, ..., r-1]
        val sumRs = sumOfArithmeticProgression(0, r)
        // sum of [0, 1, ..., l-1]
        val sumLs = sumOfArithmeticProgression(0, l)
        val expectedSum = if (l < r) sumRs - sumLs else 0
        tree.foldRange(int.fromInt(l), int.fromInt(r)) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.foldRange(int.fromInt(-1), size)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.foldRange(int.zero, size + int.one)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.foldRange(int.fromInt(-1), int.fromInt(-2)) shouldBe 0
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.foldRange(size + int.fromInt(2), size + int.one) shouldBe 0
      }

      assume(size.toLong >= 10)
      tree.foldRange(int.fromInt(10), int.fromInt(9)) shouldBe 0
      tree.foldRange(int.fromInt(10), int.fromInt(10)) shouldBe 0

    }

  def apply[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit =
    "apply" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.zero, size)) {
        tree(x) shouldBe x.toLong
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree(int.fromInt(-1))
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree(size)
      }

    }

  def updated[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit =
    "updated" in {

      // [0, 1, 2, 3, ... ]
      val int = implicitly[Integral[K]]
      var tree = tabulate(_.toLong)
      val size = tree.size

      // [0, 1, 2, 3, ... ] => [1, 2, 3, 4, ... ]
      for (x <- Iterator.range(int.zero, size)) {
        tree = tree.updated(x, 1)
      }
      for (x <- Iterator.range(int.zero, size + int.one)) {
        tree.foldUntil(x) shouldBe x.toLong
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated(int.fromInt(-1), 123)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.updated(size, 123)
      }

    }

  def asMonoidFenwickTree[K, V](
      groupFenwickTree: GroupFenwickTree[K, V]
  ): Unit =
    "asMonoidFenwickTree" in {

      val monoidFenwickTree: MonoidFenwickTree[K, V] = groupFenwickTree
      monoidFenwickTree shouldBe a[GroupFenwickTree[_, _]]

    }

}
