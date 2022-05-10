package algo.data.fenwick

import cats.kernel.CommutativeMonoid

object FenwickTreeSparseFactorySyntax {

  implicit class RichFenwickTreeSparseFactory[
      E[V] <: CommutativeMonoid[V],
      Collection[_, _]
  ](factory: FenwickTreeSparseFactory[E, Collection]) {

    def tabulateS(
        size: Long,
        f: Long => Long
    )(implicit ev: E[Long]): Collection[Long, Long] = {
      val values = (0L until size).map(i => i -> f(i))
      factory.from(size, values)
    }

    def tabulateS(
        size1: Long,
        size2: Long,
        f: (Long, Long) => Long
    )(implicit ev: E[Long]): Collection[(Long, Long), Long] = {
      val values =
        for {
          x <- 0L until size1
          y <- 0L until size2
        } yield (x, y) -> f(x, y)
      factory.from(size1, size2, values)
    }

  }

}
