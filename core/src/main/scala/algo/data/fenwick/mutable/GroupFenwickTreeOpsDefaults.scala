package algo.data.fenwick.mutable

import algo.data.fenwick

private trait GroupFenwickTreeOpsDefaults[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends GroupFenwickTreeOps[K, V, Collection]
    with fenwick.GroupFenwickTreeOpsDefaults[K, V, Collection]
    with SemigroupFenwickTreeOpsDefaults[K, V, Collection] {

  final override def update(index: K, value: V): this.type = {
    if (indexOps.outOfBoundCO(index))
      throw new IndexOutOfBoundsException(
        s"Index out of range [$zero, $size): $index"
      )
    val delta = group.remove(value, this(index))
    combine(index, delta)
    this
  }

}
