package algo.data.fenwick.immutable

import algo.data.fenwick
import algo.data.fenwick.IntFenwickTreeIndexOps
import cats.kernel.CommutativeSemigroup
import cats.syntax.semigroup._

import scala.collection.immutable

private trait FenwickTreeFactoryDefaults[
    -E[V] <: CommutativeSemigroup[V],
    +Collection[K, V] <: SemigroupFenwickTreeOps[K, V, Collection[K, V]]
] extends fenwick.FenwickTreeFactory[E, Collection] {

  override final def from[V: E](
      iterable: IterableOnce[V]
  ): Collection[Int, V] = {
    var values = immutable.Vector.from(iterable)
    val index = new IntFenwickTreeIndexOps(values.size)
    for (x <- values.indices if index.up(x) < values.size) {
      val newValue = values(index.up(x)) |+| values(x)
      values = values.updated(index.up(x), newValue)
    }
    apply(values)
  }

  protected def apply[V: E](
      values: immutable.Vector[V]
  ): Collection[Int, V]

}
