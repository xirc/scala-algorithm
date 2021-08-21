package algo.data.fenwick

import cats.kernel.CommutativeGroup

trait GroupFenwickTreeOps[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends MonoidFenwickTreeOps[K, V, Collection] {

  implicit def group: CommutativeGroup[V]

  /** Folds elements in specified range [`from`, `until`)
    *
    * @note
    *   Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def foldRange(from: K, until: K): V

  /** Gets the element at the specified index
    *
    * @note
    *   Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def apply(index: K): V

  /** Replaces element at specified index with a new value
    *
    * @note
    *   Time Complexity: O(Log)
    */
  def updated(index: K, value: V): Collection

}
