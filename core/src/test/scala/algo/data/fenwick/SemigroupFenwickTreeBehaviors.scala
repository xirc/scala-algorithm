package algo.data.fenwick

import algo.data.fenwick.TestKit.*
import algo.testing.BaseSpec

import scala.Integral.Implicits.*

trait SemigroupFenwickTreeBehaviors extends BaseSpec with FenwickTreeBehaviors {

  def semigroup[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit = {
    reduceUntil(tabulate)
    reduceTo(tabulate)
    combined(tabulate)
  }

  def reduceUntil[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit =
    "reduceUntil" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.one, size + int.one)) {
        // sum of [0, 1, ..., x-1]
        val expectedSum = sumOfArithmeticProgression(0, x.toLong)
        assert(tree.reduceUntil(x) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil(int.fromInt(0))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil(int.fromInt(-1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceUntil(size + int.one)
      }

    }

  def reduceTo[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit =
    "reduceTo" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.zero, size)) {
        // sum of [0, 1, ..., x]
        val expectedSum = sumOfArithmeticProgression(0, x.toLong + 1)
        assert(tree.reduceTo(x) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo(int.fromInt(-1))
      }

      intercept[IndexOutOfBoundsException] {
        tree.reduceTo(size)
      }

    }

  def combined[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit =
    "combined" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      var tree = tabulate(_.toLong)
      val size = tree.size

      // [ 0, 1, 2, ... ] => [ 1, 2, 3, ... ]
      for (x <- Iterator.range(int.zero, size)) {
        tree = tree.combined(x, 1)
      }

      for (x <- Iterator.range(int.one, size + int.one)) {
        // sum of [1, 2, ..., x]
        val expectedSum = sumOfArithmeticProgression(1, x.toLong + 1)
        assert(tree.reduceUntil(x) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined(int.fromInt(-1), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combined(size, 1)
      }

    }

}
