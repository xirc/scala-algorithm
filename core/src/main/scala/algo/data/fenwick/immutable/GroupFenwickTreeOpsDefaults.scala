package algo.data.fenwick.immutable

import algo.data.fenwick

private trait GroupFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends GroupFenwickTreeOps[K, V, Collection]
    with fenwick.GroupFenwickTreeOpsDefaults[K, V, Collection] {

  final override def updated(index: K, value: V): Collection = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size - 1}): $index"
      )
    val delta = group.remove(value, this(index))
    combined(index, delta)
  }

}
