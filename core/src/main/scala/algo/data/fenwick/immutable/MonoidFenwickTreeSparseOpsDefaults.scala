package algo.data.fenwick.immutable

import algo.data.fenwick

import scala.collection.immutable

private trait MonoidFenwickTreeSparseOpsDefaults[
    K,
    V,
    +Collection <: MonoidFenwickTreeOps[K, V, Collection]
] extends MonoidFenwickTreeOps[K, V, Collection]
    with fenwick.MonoidFenwickTreeOpsDefaults[K, V, Collection] {

  final override def combined(index: K, value: V): Collection = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $index"
      )
    var newValues = underlyingCollection
    indexOps.upIterator(index).foreach { idx =>
      newValues = newValues.updated(
        idx,
        monoid.combine(newValues.getOrElse(idx, monoid.empty), value)
      )
    }
    build(newValues)
  }

  @inline
  protected def build(underlying: immutable.HashMap[K, V]): Collection

  @inline
  protected def underlyingCollection: immutable.HashMap[K, V]

}
