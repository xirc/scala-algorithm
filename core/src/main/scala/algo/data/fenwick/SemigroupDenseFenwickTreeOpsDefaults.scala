package algo.data.fenwick

private trait SemigroupDenseFenwickTreeOpsDefaults[
    V,
    +Collection <: SemigroupFenwickTreeOps[Int, V, Collection]
] extends SemigroupFenwickTreeOps[Int, V, Collection]
    with SemigroupFenwickTreeOpsDefaults[Int, V, Collection] {

  final override def zero: Int = 0

}
