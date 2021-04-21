package algo.data.fenwick.mutable

import algo.data.fenwick

import scala.Integral.Implicits._

trait GroupFenwickTreeBehaviors
    extends fenwick.GroupFenwickTreeBehaviors
    with SemigroupFenwickTreeBehaviors {

  def mutableGroup[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit = {
    update(tabulate)
  }

  def update[K: Integral](
      tabulate: (K => Long) => GroupFenwickTree[K, Long]
  ): Unit =
    "update" in {

      val int = implicitly[Integral[K]]
      val tree = tabulate(_.toLong)
      val size = tree.size

      for (x <- Iterator.range(int.zero, size)) {
        tree(x) = 1
      }
      for (x <- Iterator.range(int.zero, size + int.one)) {
        tree.foldUntil(x) shouldBe x
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree(int.fromInt(-1)) = 123
      }

      a[IndexOutOfBoundsException] shouldBe thrownBy {
        tree(size) = 123
      }

    }

}
