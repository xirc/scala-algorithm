package algo.data.fenwick

private trait MonoidSparseFenwickTree2DOpsDefaults[
    V,
    +Collection <: MonoidFenwickTreeOps[(Long, Long), V, Collection]
] extends MonoidFenwickTreeOps[(Long, Long), V, Collection]
    with MonoidFenwickTreeOpsDefaults[(Long, Long), V, Collection] {

  override final def zero: (Long, Long) = (0, 0)

}
