package algo.data.dsu.immutable

import algo.data.dsu.DisjointSetUnionFactory
import cats.kernel.CommutativeSemigroup

/** @see [[algo.data.dsu.mutable.DisjointSetUnion]] */
trait DisjointSetUnion[V] extends IterableOnce[V] {

  /** Returns the number of members
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: Int

  /** Finds a value of the representative of the group containing the given
    * member index
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def find(v: Int): (V, DisjointSetUnion[V])

  /** Returns true if the same group containing the given member indices
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def isSame(u: Int, v: Int): (Boolean, DisjointSetUnion[V])

  /** Merge two groups into a single group containing the given member indices
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(a(N)) amortized
    */
  def unite(u: Int, v: Int): DisjointSetUnion[V]

}

object DisjointSetUnion extends DisjointSetUnionFactory[DisjointSetUnion] {

  override def from[V: CommutativeSemigroup](
      iterable: IterableOnce[V]
  ): DisjointSetUnion[V] =
    new DefaultDisjointSetUnion(iterable)

  override def empty[A](implicit
      evidence$6: CommutativeSemigroup[A]
  ): DisjointSetUnion[A] = {
    from(Iterable.empty)
  }

}
