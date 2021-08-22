package algo.data.fenwick.mutable

import algo.data.fenwick

trait GroupFenwickTreeOps[
    K,
    V,
    +Collection <: GroupFenwickTreeOps[K, V, Collection]
] extends fenwick.GroupFenwickTreeOps[K, V, Collection]
    with MonoidFenwickTreeOps[K, V, Collection] {

  /** Replaces element at specified index with a new value
    *
    * @note
    *   Time Complexity: O(Log)
    */
  def update(index: K, value: V): this.type

  override def updated(index: K, value: V): Collection = {
    val clone = this.clone()
    clone.update(index, value)
    clone
  }

}
