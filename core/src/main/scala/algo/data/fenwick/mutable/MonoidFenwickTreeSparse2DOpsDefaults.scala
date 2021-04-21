package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.{
  FenwickTreeIndexOps,
  LongFenwickTreeIndexOps,
  Tuple2FenwickTreeIndexOps
}

import scala.collection.mutable

private trait MonoidFenwickTreeSparse2DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[(Long, Long), V, Collection]
] extends fenwick.MonoidSparseFenwickTree2DOpsDefaults[V, Collection]
    with SemigroupFenwickTreeOpsDefaults[(Long, Long), V, Collection] {

  final override protected def indexOpsOf(
      until: (Long, Long)
  ): FenwickTreeIndexOps[(Long, Long)] =
    new Tuple2FenwickTreeIndexOps(
      new LongFenwickTreeIndexOps(until._1),
      new LongFenwickTreeIndexOps(until._2)
    )

  final override protected def getValue(index: (Long, Long)): V =
    underlyingCollection.getOrElse(index, monoid.empty)

  final override protected def updateValue(
      index: (Long, Long),
      value: V
  ): Unit =
    underlyingCollection(index) = value

  protected def underlyingCollection: mutable.HashMap[(Long, Long), V]

}
