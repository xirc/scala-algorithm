package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.testing.BaseSpec
import cats.instances.long._

final class SemigroupFenwickTreeSpec
    extends BaseSpec
    with fenwick.FenwickTreeFactoryBehaviors
    with fenwick.SemigroupFenwickTreeBehaviors {

  "immutable|SemigroupFenwickTree" should {

    behave like factory(SemigroupFenwickTree)
    behave like treeHasSize1D(SemigroupFenwickTree)
    behave like semigroup(
      SemigroupFenwickTree.tabulate(size = 1_000)
    )

  }

}
