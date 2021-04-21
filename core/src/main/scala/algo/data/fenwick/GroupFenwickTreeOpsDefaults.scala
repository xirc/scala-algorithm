package algo.data.fenwick

import cats.kernel.CommutativeMonoid

private trait GroupFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends GroupFenwickTreeOps[K, V, Collection]
    with MonoidFenwickTreeOpsDefaults[K, V, Collection] {

  final override implicit def monoid: CommutativeMonoid[V] = group

  final override def foldRange(from: K, until: K): V = {
    if (indexOps.outOfBoundCO(from))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $from"
      )
    if (indexOps.outOfBoundCC(until))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size]: $until"
      )
    indexOps.boundIterator(from, until).foldLeft(group.empty) {
      case (acc, (eachUntil, sign)) =>
        if (sign >= 0) {
          group.combine(acc, foldUntil(eachUntil))
        } else {
          group.remove(acc, foldUntil(eachUntil))
        }
    }
  }

  final override def apply(index: K): V = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $index"
      )
    foldRange(index, indexOps.nextUpperBound(index))
  }

}
