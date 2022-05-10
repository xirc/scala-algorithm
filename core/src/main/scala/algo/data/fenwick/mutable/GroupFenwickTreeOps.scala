package algo.data.fenwick.mutable

import algo.data.fenwick

trait GroupFenwickTreeOps[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends fenwick.GroupFenwickTreeOps[K, V, Collection]
    with MonoidFenwickTreeOps[K, V, Collection] {

  /** Updates the element at the given index to the given value
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `index` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def update(index: K, value: V): this.type

  override def updated(index: K, value: V): Collection = {
    val clone = this.clone()
    clone.update(index, value)
    clone
  }

}
