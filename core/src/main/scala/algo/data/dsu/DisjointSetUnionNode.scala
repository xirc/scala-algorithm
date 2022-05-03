package algo.data.dsu

import cats.kernel.CommutativeSemigroup
import cats.syntax.semigroup.*

private final case class DisjointSetUnionNode[V](
    leader: Int,
    rank: Int,
    value: V
)

private object DisjointSetUnionNode {

  def unite[V: CommutativeSemigroup](
      leader: DisjointSetUnionNode[V],
      follower: DisjointSetUnionNode[V]
  ): (DisjointSetUnionNode[V], DisjointSetUnionNode[V]) = {
    assert(leader.rank >= follower.rank)
    val newFollower = follower.copy(leader = leader.leader)
    val newLeader = {
      val newValue =
        leader.value |+| follower.value
      val newRank =
        if (leader.rank == follower.rank) {
          leader.rank + 1
        } else {
          leader.rank
        }
      leader.copy(rank = newRank, value = newValue)
    }
    (newLeader, newFollower)
  }

}
