package algo.data.fenwick.immutable

import cats.kernel.CommutativeMonoid

import scala.collection.immutable

private final class MonoidFenwickTreeSparse2D[V](
    _size: (Long, Long),
    _values: immutable.HashMap[(Long, Long), V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[(Long, Long), V]
    with MonoidFenwickTreeOps[(Long, Long), V, MonoidFenwickTreeSparse2D[V]]
    with MonoidFenwickTreeSparse2DOpsDefaults[V, MonoidFenwickTreeSparse2D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: (Long, Long) = _size

  override protected def build(
      values: immutable.HashMap[(Long, Long), V]
  ): MonoidFenwickTreeSparse2D[V] =
    new MonoidFenwickTreeSparse2D(size, values, monoid)

  override protected def underlyingCollection
      : immutable.HashMap[(Long, Long), V] =
    _values

}
