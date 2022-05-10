package algo.data.fenwick

import cats.kernel.CommutativeGroup

trait GroupFenwickTreeOps[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends MonoidFenwickTreeOps[K, V, Collection] {

  implicit def group: CommutativeGroup[V]

  /** Folds elements in the given range [`from`, `until`)
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   - if `from` is less than `zero` or greater than or equal to `size`
    *   - if `until` is less than `zero` or greater than `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def foldRange(from: K, until: K): V

  /** Returns the element at the given index
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `index` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def apply(index: K): V

  /** Updates the element at the given index to the given value
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `index` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def updated(index: K, value: V): Collection

}
