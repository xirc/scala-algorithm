package algo.data.dsu.immutable

import cats.data.State

trait DisjointSetUnionSyntax {

  /** @see [[DisjointSetUnion.size]] */
  def size[V]: State[DisjointSetUnion[V], Int] = State.inspect { s =>
    s.size
  }

  /** @see [[DisjointSetUnion.find]] */
  def find[V](v: Int): State[DisjointSetUnion[V], V] = State { s =>
    s.find(v).swap
  }

  /** @see [[DisjointSetUnion.isSame]] */
  def isSame[V](u: Int, v: Int): State[DisjointSetUnion[V], Boolean] = State {
    s =>
      s.isSame(u, v).swap
  }

  /** @see [[DisjointSetUnion.unite]] */
  def unite[V](u: Int, v: Int): State[DisjointSetUnion[V], Unit] =
    State.modify { s =>
      s.unite(u, v)
    }

  /** @see [[DisjointSetUnion.iterator]] */
  def iterator[V]: State[DisjointSetUnion[V], Iterator[V]] = State.inspect {
    s =>
      s.iterator
  }

  /** @see [[DisjointSetUnion.knownSize]] */
  def knownSize[V]: State[DisjointSetUnion[V], Int] = State.inspect { s =>
    s.knownSize
  }

}
