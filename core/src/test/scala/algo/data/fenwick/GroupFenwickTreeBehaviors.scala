package algo.data.fenwick

import algo.data.fenwick.TestKit.*
import algo.testing.BaseSpec

import scala.Integral.Implicits.*
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
        assert(tree.foldRange(int.fromInt(l), int.fromInt(r)) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange(int.fromInt(-1), size)
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange(int.zero, size + int.one)
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange(int.fromInt(-1), int.fromInt(-2))
      }

      intercept[IndexOutOfBoundsException] {
        tree.foldRange(size + int.fromInt(2), size + int.one)
      }

      assume(size.toLong >= 10)
      assert(tree.foldRange(int.fromInt(10), int.fromInt(9)) === 0L)
      assert(tree.foldRange(int.fromInt(10), int.fromInt(10)) === 0L)

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
        assert(tree(x) === x.toLong)
      }

      intercept[IndexOutOfBoundsException] {
        tree(int.fromInt(-1))
      }

      intercept[IndexOutOfBoundsException] {
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
        assert(tree.foldUntil(x) === x.toLong)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated(int.fromInt(-1), 123)
      }

      intercept[IndexOutOfBoundsException] {
        tree.updated(size, 123)
      }

    }

  def asMonoidFenwickTree[K, V](
      groupFenwickTree: GroupFenwickTree[K, V]
  ): Unit =
    "asMonoidFenwickTree" in {

      val monoidFenwickTree: MonoidFenwickTree[K, V] = groupFenwickTree
      assert(monoidFenwickTree.isInstanceOf[GroupFenwickTree[?, ?]])

    }

}
