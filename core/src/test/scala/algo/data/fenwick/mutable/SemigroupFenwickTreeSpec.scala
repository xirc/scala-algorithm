package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long.*

final class SemigroupFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeFactoryBehaviors
    with SemigroupFenwickTreeBehaviors {

  "mutable|SemigroupFenwickTree" should {

    behave like factory(SemigroupFenwickTree)
    behave like treeHasSize1D(SemigroupFenwickTree)
    behave like semigroup(
      SemigroupFenwickTree.tabulate(size = 1_000)
    )
    behave like mutableSemigroup(
      SemigroupFenwickTree.tabulate(size = 1_000)
    )

  }

}
