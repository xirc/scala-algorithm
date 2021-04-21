package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.data.fenwick.{
  FenwickTreeIndexOps,
  LongFenwickTreeIndexOps,
  Tuple2FenwickTreeIndexOps
}

private trait MonoidFenwickTreeSparse2DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[(Long, Long), V, Collection]
] extends fenwick.MonoidSparseFenwickTree2DOpsDefaults[V, Collection]
    with MonoidFenwickTreeSparseOpsDefaults[(Long, Long), V, Collection] {

  final override protected def indexOpsOf(
      until: (Long, Long)
  ): FenwickTreeIndexOps[(Long, Long)] =
    new Tuple2FenwickTreeIndexOps(
      new LongFenwickTreeIndexOps(until._1),
      new LongFenwickTreeIndexOps(until._2)
    )

  final override protected def getValue(index: (Long, Long)): V =
    underlyingCollection.getOrElse(index, monoid.empty)

}
