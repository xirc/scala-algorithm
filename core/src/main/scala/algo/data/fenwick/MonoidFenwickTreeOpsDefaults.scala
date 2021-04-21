package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

private trait MonoidFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: MonoidFenwickTreeOps[K, V, Collection]
] extends MonoidFenwickTreeOps[K, V, Collection]
    with SemigroupFenwickTreeOpsDefaults[K, V, Collection] {

  final override implicit def semigroup: CommutativeSemigroup[V] = monoid

  final override def foldUntil(until: K): V = {
    if (indexOps.outOfBoundCC(until))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size]: $until"
      )
    val to = indexOps.nextLowerBound(until)
    indexOps
      .downIterator(to)
      .map(getValue)
      .fold(monoid.empty)((acc, value) => semigroup.combine(acc, value))
  }

  final override def foldTo(to: K): V = {
    val until = indexOps.nextUpperBound(to)
    foldUntil(until)
  }

}
