package algo.data.fenwick.mutable

import cats.kernel.CommutativeMonoid

import scala.collection.mutable

private final class MonoidFenwickTreeSparse2D[V](
    _size: (Long, Long),
    _values: mutable.HashMap[(Long, Long), V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[(Long, Long), V]
    with MonoidFenwickTreeOps[(Long, Long), V, MonoidFenwickTreeSparse2D[V]]
    with MonoidFenwickTreeSparse2DOpsDefaults[V, MonoidFenwickTreeSparse2D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: (Long, Long) = _size

  override protected def underlyingCollection
      : mutable.HashMap[(Long, Long), V] = _values

  override def clone(): MonoidFenwickTreeSparse2D[V] =
    new MonoidFenwickTreeSparse2D(size, _values.clone(), monoid)

}
