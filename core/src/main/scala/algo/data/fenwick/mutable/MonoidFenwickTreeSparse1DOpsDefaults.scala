package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.{FenwickTreeIndexOps, LongFenwickTreeIndexOps}

import scala.collection.mutable

private trait MonoidFenwickTreeSparse1DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[Long, V, Collection]
] extends fenwick.MonoidSparseFenwickTree1DOpsDefaults[V, Collection]
    with SemigroupFenwickTreeOpsDefaults[Long, V, Collection] {

  final override protected def indexOpsOf(
      until: Long
  ): FenwickTreeIndexOps[Long] =
    new LongFenwickTreeIndexOps(until)

  final override protected def getValue(index: Long): V =
    underlyingCollection.getOrElse(index, monoid.empty)

  final override protected def updateValue(index: Long, value: V): Unit =
    underlyingCollection(index) = value

  protected def underlyingCollection: mutable.HashMap[Long, V]

}
