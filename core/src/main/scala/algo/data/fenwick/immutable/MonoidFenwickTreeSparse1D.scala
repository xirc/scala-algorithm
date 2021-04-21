package algo.data.fenwick.immutable

import cats.kernel.CommutativeMonoid

import scala.collection.immutable

private final class MonoidFenwickTreeSparse1D[V](
    _size: Long,
    _values: immutable.HashMap[Long, V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[Long, V]
    with MonoidFenwickTreeOps[Long, V, MonoidFenwickTreeSparse1D[V]]
    with MonoidFenwickTreeSparse1DOpsDefaults[V, MonoidFenwickTreeSparse1D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: Long = _size

  override protected def build(
      values: immutable.HashMap[Long, V]
  ): MonoidFenwickTreeSparse1D[V] =
    new MonoidFenwickTreeSparse1D(size, values, monoid)

  override protected def underlyingCollection: immutable.HashMap[Long, V] =
    _values

}
