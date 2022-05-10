package algo.data.fenwick.mutable

import algo.data.fenwick

trait SemigroupFenwickTreeOps[
    K,
    V,
    +Collection <: SemigroupFenwickTreeOps[K, V, Collection]
] extends fenwick.SemigroupFenwickTreeOps[K, V, Collection] {

  override def combined(index: K, value: V): Collection = {
    val newInstance = this.clone()
    newInstance.combine(index, value)
    newInstance
  }

  /** Updates the element at the given index to the given value
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if `index` is less than `zero` or greater than or equal to `size`
    *
    * @note
    *   Time Complexity: O(log N)
    */
  def combine(index: K, value: V): this.type

  override def clone(): Collection =
    super.clone().asInstanceOf[Collection]

}
