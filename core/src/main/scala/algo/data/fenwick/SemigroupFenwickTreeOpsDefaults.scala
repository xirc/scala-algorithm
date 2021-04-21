package algo.data.fenwick

private trait SemigroupFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] extends SemigroupFenwickTreeOps[K, V, Collection] {

  final override def reduceUntil(until: K): V = {
    val to = indexOps.nextLowerBound(until)
    if (indexOps.outOfBoundCO(to))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): lower($until)"
      )
    indexOps
      .downIterator(to)
      .map(getValue)
      .reduce((acc, value) => semigroup.combine(acc, value))
  }

  final override def reduceTo(to: K): V = {
    val until = indexOps.nextUpperBound(to)
    reduceUntil(until)
  }

  protected def indexOpsOf(until: K): FenwickTreeIndexOps[K]

  protected final def indexOps: FenwickTreeIndexOps[K] =
    indexOpsOf(size)

  protected def getValue(index: K): V

}
