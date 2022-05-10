package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

trait SemigroupFenwickTreeOps[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] {

  implicit def semigroup: CommutativeSemigroup[V]

  /** Returns the zero
    *
    * @note
    *   Time Complexity: O(1)
    */
  def zero: K

  /** Returns the size of this tree
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: K

  /** Reduces elements in the given range [`zero`, `until`)
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `until` is less than `zero` or greater than `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def reduceUntil(until: K): V

  /** Reduces elements in the given range [`zero`, `to`]
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `to` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def reduceTo(to: K): V

  /** Updates the element at the given index to the given value
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `index` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def combined(index: K, value: V): Collection

}
