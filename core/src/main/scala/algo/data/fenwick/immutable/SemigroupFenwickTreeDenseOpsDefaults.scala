package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.data.fenwick.{
  FenwickTreeIndexOps,
  IntFenwickTreeIndexOps,
  SemigroupFenwickTreeOpsDefaults
}
import cats.syntax.semigroup.*

import scala.collection.immutable

private trait SemigroupFenwickTreeDenseOpsDefaults[
    V,
    +Collection <: SemigroupFenwickTreeOps[Int, V, Collection]
] extends SemigroupFenwickTreeOps[Int, V, Collection]
    with SemigroupFenwickTreeOpsDefaults[Int, V, Collection]
    with fenwick.SemigroupDenseFenwickTreeOpsDefaults[V, Collection] {

  final override def combined(index: Int, value: V): Collection = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $index"
      )
    var newValues = underlyingCollection
    indexOps.upIterator(index).foreach { idx =>
      newValues = newValues.updated(idx, newValues(idx) |+| value)
    }
    build(newValues)
  }

  final override protected def getValue(index: Int): V =
    underlyingCollection(index)

  final override protected def indexOpsOf(
      until: Int
  ): FenwickTreeIndexOps[Int] =
    new IntFenwickTreeIndexOps(until)

  @inline
  protected def build(underlying: immutable.Vector[V]): Collection

  @inline
  protected def underlyingCollection: immutable.Vector[V]

}
