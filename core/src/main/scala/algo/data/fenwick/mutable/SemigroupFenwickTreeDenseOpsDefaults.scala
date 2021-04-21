package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.{FenwickTreeIndexOps, IntFenwickTreeIndexOps}

import scala.collection.mutable

private trait SemigroupFenwickTreeDenseOpsDefaults[
    V,
    +Collection <: SemigroupFenwickTreeOps[Int, V, Collection]
] extends SemigroupFenwickTreeOpsDefaults[Int, V, Collection]
    with fenwick.SemigroupDenseFenwickTreeOpsDefaults[V, Collection] {

  final override protected def indexOpsOf(
      until: Int
  ): FenwickTreeIndexOps[Int] =
    new IntFenwickTreeIndexOps(until)

  final override protected def updateValue(index: Int, value: V): Unit =
    underlyingCollection(index) = value

  final override protected def getValue(index: Int): V =
    underlyingCollection(index)

  protected def underlyingCollection: mutable.ArrayBuffer[V]

}
