package algo.data.dsu

import cats.kernel.CommutativeSemigroup

import scala.collection.Iterable

trait DisjointSetUnionFactory[Collection[_]] {

  def from[V: CommutativeSemigroup](
      iterable: IterableOnce[V]
  ): Collection[V]

  def apply[V: CommutativeSemigroup](xs: V*): Collection[V] =
    from(xs)

  def iterate[V: CommutativeSemigroup](start: V, len: Int)(
      f: V => V
  ): Collection[V] =
    from(Iterator.iterate(start, len)(f))

  def unfold[V: CommutativeSemigroup, S](init: S)(
      f: S => Option[(V, S)]
  ): Collection[V] =
    from(Iterator.unfold(init)(f))

  def concat[V: CommutativeSemigroup](xss: Iterable[V]*): Collection[V] =
    from(Iterator.concat(xss*))

  def fill[V: CommutativeSemigroup](size: Int)(
      elem: => V
  ): Collection[V] =
    from(Iterator.fill(size)(elem))

  def tabulate[V: CommutativeSemigroup](size: Int)(
      f: Int => V
  ): Collection[V] =
    from(Iterator.tabulate(size)(f))

}
