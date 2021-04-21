package algo.data.fenwick

trait FenwickTreeFactoryProxy[-E[V], +Collection[X, V]]
    extends FenwickTreeFactory[E, Collection] {

  protected def factory: FenwickTreeFactory[E, Collection]

  final override def from[V: E](iterable: IterableOnce[V]): Collection[Int, V] =
    factory.from(iterable)

}
