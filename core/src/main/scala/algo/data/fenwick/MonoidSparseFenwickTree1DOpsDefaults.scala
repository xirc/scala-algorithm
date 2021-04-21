package algo.data.fenwick

private trait MonoidSparseFenwickTree1DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[Long, V, Collection]
] extends MonoidFenwickTreeOpsDefaults[Long, V, Collection] {

  final override def zero: Long = 0

}
