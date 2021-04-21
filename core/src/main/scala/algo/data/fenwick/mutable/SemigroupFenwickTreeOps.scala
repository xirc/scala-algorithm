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

  /** Combines the element at specified index and specified value
    *
    * @note Time Complexity: O(Log)
    */
  @throws[IndexOutOfBoundsException]
  def combine(index: K, value: V): Unit

  override def clone(): Collection =
    super.clone().asInstanceOf[Collection]

}
