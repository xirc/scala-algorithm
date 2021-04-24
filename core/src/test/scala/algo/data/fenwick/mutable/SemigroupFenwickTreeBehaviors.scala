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
        tree.combine(x, 1) shouldBe tree
      }

      for (x <- Iterator.range(int.one, size + int.one)) {
        // sum of [1, 2, ..., x]
        val expectedSum = x.toLong * (x.toLong + 1) / 2
        tree.reduceUntil(x) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combine(int.fromInt(-1), 1)
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.combine(size, 1)
      }

    }

}
