package algo.data.fenwick.mutable

import algo.data.fenwick

private trait SemigroupFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] extends SemigroupFenwickTreeOps[K, V, Collection]
    with fenwick.SemigroupFenwickTreeOpsDefaults[K, V, Collection] {

  final override def combine(index: K, value: V): Unit = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $index"
      )
    indexOps.upIterator(index).foreach { idx =>
      updateValue(idx, semigroup.combine(getValue(idx), value))
    }
  }

  @inline
  protected def updateValue(index: K, value: V): Unit

}
