package algo.data.dsu.immutable

import cats.data.State

trait DisjointSetUnionSyntax {

  /** Size of the given [[DisjointSetUnion]] */
  def size[V]: State[DisjointSetUnion[V], Int] = State.inspect { s =>
    s.size
  }

  /** Finds a value of the given element */
  def find[V](v: Int): State[DisjointSetUnion[V], V] = State { s =>
    s.find(v).swap
  }

  /** Checks whether given elements belong to the same group */
  def isSame[V](u: Int, v: Int): State[DisjointSetUnion[V], Boolean] = State {
    s =>
      s.isSame(u, v).swap
  }

  /** Checks whether given elements belong to the same group */
  def unite[V](u: Int, v: Int): State[DisjointSetUnion[V], Unit] =
    State.modify { s =>
      s.united(u, v)
    }

}
