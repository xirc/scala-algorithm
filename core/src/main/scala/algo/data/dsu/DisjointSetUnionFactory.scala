package algo.data.dsu

import cats.kernel.CommutativeSemigroup

import scala.collection.{EvidenceIterableFactory, Iterable}
import scala.collection.mutable

trait DisjointSetUnionFactory[Collection[_]]
    extends EvidenceIterableFactory[Collection, CommutativeSemigroup] {

  /** Concatenates all iterables into a single collection */
  def concat[V: CommutativeSemigroup](xss: Iterable[V]*): Collection[V] =
    from(Iterator.concat(xss*))

  override def newBuilder[V: CommutativeSemigroup]
      : mutable.Builder[V, Collection[V]] = {
    new mutable.Builder[V, Collection[V]]() {
      val buffer: mutable.ArrayBuffer[V] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): Collection[V] = {
        from(buffer)
      }
      override def addOne(elem: V): this.type = {
        buffer.addOne(elem)
        this
      }
    }
  }

}
