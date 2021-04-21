package algo.data.fenwick

import algo.data.fenwick.TestKit._
import algo.testing.BaseSpec

import scala.Integral.Implicits._

trait MonoidFenwickTreeBehaviors
    extends BaseSpec
    with SemigroupFenwickTreeBehaviors {

  def monoid[K: Integral](
      tabulate: (K => Long) => MonoidFenwickTree[K, Long]
  ): Unit = {
    foldUntil(tabulate)
    foldTo(tabulate)
    asSemigroupFenwickTree(tabulate(_.toLong))
  }

  def foldUntil[K: Integral](
      tabulate: (K => Long) => MonoidFenwickTree[K, Long]
  ): Unit =
    "foldUntil" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.zero, size + int.one)) {
        // sum of [0, 1, ..., x-1]
        val expectedSum = sumOfArithmeticProgression(0, x.toLong)
        tree.foldUntil(x) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.reduceUntil(int.fromInt(-1))
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.reduceUntil(size + int.one)
      }

    }

  def foldTo[K: Integral](
      tabulate: (K => Long) => MonoidFenwickTree[K, Long]
  ): Unit =
    "foldTo" in {

      // [ 0, 1, 2, ... ]
      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.zero, size)) {
        // sum of [0, 1, ..., x]
        val expectedSum = sumOfArithmeticProgression(0, x.toLong + 1)
        tree.foldTo(x) shouldBe expectedSum
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.reduceTo(int.fromInt(-1))
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree.reduceTo(size)
      }

    }

  def asSemigroupFenwickTree[K, V](
      monoidFenwickTree: MonoidFenwickTree[K, V]
  ): Unit =
    "asSemigroupFenwickTree" in {

      val semigroupFenwickTree: SemigroupFenwickTree[K, V] = monoidFenwickTree
      semigroupFenwickTree shouldBe a[MonoidFenwickTree[_, _]]

    }

}
