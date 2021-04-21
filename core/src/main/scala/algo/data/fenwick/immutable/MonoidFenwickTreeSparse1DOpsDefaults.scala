package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.data.fenwick.{FenwickTreeIndexOps, LongFenwickTreeIndexOps}

private trait MonoidFenwickTreeSparse1DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[Long, V, Collection]
] extends fenwick.MonoidSparseFenwickTree1DOpsDefaults[V, Collection]
    with MonoidFenwickTreeSparseOpsDefaults[Long, V, Collection] {

  final override protected def getValue(index: Long): V =
    underlyingCollection.getOrElse(index, monoid.empty)

  final override protected def indexOpsOf(
      until: Long
  ): FenwickTreeIndexOps[Long] =
    new LongFenwickTreeIndexOps(until)

}
