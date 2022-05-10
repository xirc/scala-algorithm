package algo.data.fenwick

import cats.kernel.CommutativeMonoid

trait MonoidFenwickTreeOps[
    K,
    V,
    +Collection <: MonoidFenwickTreeOps[K, V, Collection]
] extends SemigroupFenwickTreeOps[K, V, Collection] {

  implicit def monoid: CommutativeMonoid[V]

  /** Folds elements in the given range [`zero`, `until`)
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `until` is less than `zero` or greater than `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def foldUntil(until: K): V

  /** Folds elements in the given range [`zero`, `to`]
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `to` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def foldTo(to: K): V

}
