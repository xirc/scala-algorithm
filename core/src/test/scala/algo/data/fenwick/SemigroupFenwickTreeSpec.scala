package algo.data.fenwick

import algo.testing.BaseSpec
import cats.instances.long._

final class SemigroupFenwickTreeSpec
    extends BaseSpec
    with FenwickTreeFactoryBehaviors
    with SemigroupFenwickTreeBehaviors {

  "SemigroupFenwickTree|default" in {

    val instance = SemigroupFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[immutable.SemigroupFenwickTree[Int, _]])

  }

  "SemigroupFenwickTree|mutable" in {

    val instance: SemigroupFenwickTree[Int, Long] =
      mutable.SemigroupFenwickTree.tabulate(size = 10)(_.toLong)
    assert(instance.isInstanceOf[mutable.SemigroupFenwickTree[_, _]])

  }

  "SemigroupFenwickTree" should {

    behave like factory(SemigroupFenwickTree)
    behave like treeHasSize1D(SemigroupFenwickTree)
    behave like semigroup(
      SemigroupFenwickTree.tabulate(size = 1000)
    )

  }

}
