package algo.data.fenwick.mutable

import algo.data.fenwick
import algo.data.fenwick.IntFenwickTreeIndexOps
import cats.kernel.CommutativeSemigroup
import cats.syntax.semigroup._

import scala.collection.mutable

private trait FenwickTreeFactoryDefaults[
    -E[V] <: CommutativeSemigroup[V],
    +Collection[K, V] <: SemigroupFenwickTreeOps[K, V, Collection[K, V]]
] extends fenwick.FenwickTreeFactory[E, Collection] {

  final override def from[V: E](
      iterable: IterableOnce[V]
  ): Collection[Int, V] = {
    val values = mutable.ArrayBuffer.from(iterable)
    val index = new IntFenwickTreeIndexOps(values.size)
    for (x <- values.indices if index.up(x) < values.size) {
      val newValue = values(index.up(x)) |+| values(x)
      values(index.up(x)) = newValue
    }
    apply(values)
  }

  protected def apply[V: E](
      values: mutable.ArrayBuffer[V]
  ): Collection[Int, V]

}
