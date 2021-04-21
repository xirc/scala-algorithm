package algo.data.fenwick

import scala.collection.Iterable

trait FenwickTreeFactory[-E[V], +Collection[X, V]] {

  def from[V: E](iterable: IterableOnce[V]): Collection[Int, V]

  def apply[V: E](xs: V*): Collection[Int, V] =
    from(xs)

  def iterate[V: E](start: V, len: Int)(f: V => V): Collection[Int, V] =
    from(Iterator.iterate(start, len)(f))

  def unfold[V: E, S](init: S)(f: S => Option[(V, S)]): Collection[Int, V] =
    from(Iterator.unfold(init)(f))

  def range[V: Integral: E](start: V, end: V): Collection[Int, V] =
    from(Iterator.range(start, end))

  def range[V: Integral: E](start: V, end: V, step: V): Collection[Int, V] =
    from(Iterator.range(start, end, step))

  def concat[V: E](xss: Iterable[V]*): Collection[Int, V] =
    from(Iterator.concat(xss: _*))

  def fill[V: E](size: Int)(elem: => V): Collection[Int, V] =
    from(Iterator.fill(size)(elem))

  def tabulate[V: E](size: Int)(f: Int => V): Collection[Int, V] =
    from(Iterator.tabulate(size)(f))

}
