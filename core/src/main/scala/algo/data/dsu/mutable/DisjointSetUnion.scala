package algo.data.dsu.mutable

import algo.data.dsu.DisjointSetUnionFactory
import cats.kernel.CommutativeSemigroup

/** Disjoint-Set Data Structure (a.k.a. Union-Find)
  *
  * Manages a collection of disjoint sets. Each set contains no duplicate
  * members. Like an array, an index identifies each member.
  *
  * Provides some operations on almost constant time:
  *   - Determines whether the two members are in the same group
  *   - Finds a value of the representative member of the group containing the
  *     given members
  *   - Merges two sets into a single group containing the given members
  *
  * If two sets are merged into a new single set, the representative member's
  * value of the new set is calculated from the representative members' values
  * of the two sets to merge. For this calculation,
  * [[cats.kernel.CommutativeSemigroup]] must be available.
  *
  * Expects that the semigroup-combine calculation will complete in a constant
  * time, but not required. Suppose the calculation completes in O(K). The time
  * complexity of almost operation are multiplied by such time O(K).
  *
  * @tparam V
  *   Type of the members in the sets
  */
trait DisjointSetUnion[V] extends IterableOnce[V] {

  /** Returns the number of members
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: Int

  /** Alias of [[find]] */
  def apply(v: Int): V

  /** Finds a value of the representative of the group containing the given
    * member index
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def find(v: Int): V

  /** Returns true if the same group contains the given member indices
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def isSame(u: Int, v: Int): Boolean

  /** Merge two groups into a single group containing the given member indices
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def unite(u: Int, v: Int): this.type

}

object DisjointSetUnion extends DisjointSetUnionFactory[DisjointSetUnion] {

  override def from[V: CommutativeSemigroup](
      iterable: IterableOnce[V]
  ): DisjointSetUnion[V] =
    new DefaultDisjointSetUnion(iterable)

  override def empty[V: CommutativeSemigroup]: DisjointSetUnion[V] = {
    from(Iterable.empty)
  }

}
