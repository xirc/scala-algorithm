package algo.data.fenwick.mutable

import algo.data.fenwick

import scala.Integral.Implicits._

trait SemigroupFenwickTreeBehaviors
    extends fenwick.SemigroupFenwickTreeBehaviors {

  def mutableSemigroup[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit = {
    combine(tabulate)
  }

  def combine[K: Integral](
      tabulate: (K => Long) => SemigroupFenwickTree[K, Long]
  ): Unit =
    "combine" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      // [ 1, 2, 3, ... ]
      for (x <- Iterator.range(int.zero, size)) {
        assert(tree.combine(x, 1) === tree)
      }

      for (x <- Iterator.range(int.one, size + int.one)) {
        // sum of [1, 2, ..., x]
        val expectedSum = x.toLong * (x.toLong + 1) / 2
        assert(tree.reduceUntil(x) === expectedSum)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combine(int.fromInt(-1), 1)
      }

      intercept[IndexOutOfBoundsException] {
        tree.combine(size, 1)
      }

    }

}
