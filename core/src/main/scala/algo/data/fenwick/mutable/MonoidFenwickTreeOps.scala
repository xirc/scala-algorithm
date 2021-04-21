package algo.data.fenwick.mutable

import algo.data.fenwick

trait MonoidFenwickTreeOps[
    K,
    V,
    +Collection <: MonoidFenwickTreeOps[K, V, Collection]
] extends fenwick.MonoidFenwickTreeOps[K, V, Collection]
    with SemigroupFenwickTreeOps[K, V, Collection]
