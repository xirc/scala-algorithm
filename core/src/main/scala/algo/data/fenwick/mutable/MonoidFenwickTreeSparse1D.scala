package algo.data.fenwick.mutable

import cats.kernel.CommutativeMonoid

import scala.collection.mutable

private final class MonoidFenwickTreeSparse1D[V](
    _size: Long,
    _values: mutable.HashMap[Long, V],
    ev: CommutativeMonoid[V]
) extends MonoidFenwickTree[Long, V]
    with MonoidFenwickTreeOps[Long, V, MonoidFenwickTreeSparse1D[V]]
    with MonoidFenwickTreeSparse1DOpsDefaults[V, MonoidFenwickTreeSparse1D[V]] {

  override implicit def monoid: CommutativeMonoid[V] = ev

  override def size: Long = _size

  override def clone(): MonoidFenwickTreeSparse1D[V] =
    new MonoidFenwickTreeSparse1D(size, _values.clone(), monoid)

  override protected def underlyingCollection: mutable.HashMap[Long, V] =
    _values

}
