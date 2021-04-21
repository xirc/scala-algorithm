package algo.data.fenwick.immutable

import algo.data.fenwick

trait SemigroupFenwickTreeOps[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] extends fenwick.SemigroupFenwickTreeOps[K, V, Collection] {}
