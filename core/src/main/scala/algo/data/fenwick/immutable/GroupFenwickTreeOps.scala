package algo.data.fenwick.immutable

import algo.data.fenwick

trait GroupFenwickTreeOps[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends fenwick.GroupFenwickTreeOps[K, V, Collection]
    with MonoidFenwickTreeOps[K, V, Collection] {}
