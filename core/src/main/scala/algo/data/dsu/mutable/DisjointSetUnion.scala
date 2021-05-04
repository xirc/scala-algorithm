package algo.data.dsu.mutable

import algo.data.dsu.DisjointSetUnionFactory
import cats.kernel.CommutativeSemigroup

trait DisjointSetUnion[V] {

  /** Size of the [[DisjointSetUnion]]
    * @note Time Complexity: O(1)
    */
  def size: Int

  /** Finds a value of the given element
    * @note Time Complexity: O(a(N))
    */
  @throws[IndexOutOfBoundsException]
  def apply(v: Int): V

  /** Finds a value of the given element
    * @note Time Complexity: O(a(N))
    */
  @throws[IndexOutOfBoundsException]
  def find(v: Int): V

  /** Checks whether given elements belong to the same group
    * @note Time Complexity: O(a(N))
    */
  @throws[IndexOutOfBoundsException]
  def isSame(u: Int, v: Int): Boolean

  /** Makes given elements to belong to the same group
    * @note Time Complexity: O(a(N))
    */
  @throws[IndexOutOfBoundsException]
  def unite(u: Int, v: Int): this.type

}

object DisjointSetUnion extends DisjointSetUnionFactory[DisjointSetUnion] {

  override def from[V: CommutativeSemigroup](
      iterable: IterableOnce[V]
  ): DisjointSetUnion[V] =
    new DefaultDisjointSetUnion(iterable)

}
