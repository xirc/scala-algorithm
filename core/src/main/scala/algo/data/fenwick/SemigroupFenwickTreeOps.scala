package algo.data.fenwick

import cats.kernel.CommutativeSemigroup

trait SemigroupFenwickTreeOps[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] {

  implicit def semigroup: CommutativeSemigroup[V]

  /** Zero Bound
    *
    * @note
    *   Time Complexity: O(1)
    */
  def zero: K

  /** Size of Tree
    *
    * Upper Bound
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: K

  /** Reduces elements in specified range [`zero`, `until`)
    *
    * @note
    *   Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def reduceUntil(until: K): V

  /** Reduces elements in specified range [`zero`, `to`]
    *
    * @note
    *   Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def reduceTo(to: K): V

  /** Combines the element at specified index and specified value
    *
    * @note
    *   Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def combined(index: K, value: V): Collection

}
