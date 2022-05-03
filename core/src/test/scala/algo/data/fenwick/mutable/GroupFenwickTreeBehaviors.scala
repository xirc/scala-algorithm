package algo.data.fenwick.mutable

import algo.data.fenwick

import scala.Integral.Implicits.*

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
        assert((tree(x) = 1) === tree)
      }
      for (x <- Iterator.range(int.zero, size + int.one)) {
        assert(tree.foldUntil(x) === x.toLong)
      }

      intercept[IndexOutOfBoundsException] {
        tree(int.fromInt(-1)) = 123
      }

      intercept[IndexOutOfBoundsException] {
        tree(size) = 123
      }

    }

}
