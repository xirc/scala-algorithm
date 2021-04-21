package algo.data.fenwick

import cats.kernel.CommutativeMonoid

trait MonoidFenwickTreeOps[
    K,
    V,
    +Collection <: MonoidFenwickTreeOps[K, V, Collection]
] extends SemigroupFenwickTreeOps[K, V, Collection] {

  implicit def monoid: CommutativeMonoid[V]

  /** Folds elements in specified range [`zero`, `until`)
    *
    * @note Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def foldUntil(until: K): V

  /** Folds elements in specified range [`zero`, `to`]
    *
    * @note Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def foldTo(to: K): V

}
