package algo.data.bloom.immutable

import algo.data.bloom.{BloomFilterSettings, HashFunctionFactory}

private final class DefaultBloomFilter[A] private (
    settings: BloomFilterSettings,
    hashFunctions: IndexedSeq[A => Int],
    _size: Int,
    buffer: ImmutableBitSet
) extends BloomFilter[A] {
  assert(hashFunctions.size == settings.numOfHashFunctions)
  assert(_size >= 0)

  override def capacity: Int =
    settings.capacity

  override def tolerance: Double =
    settings.tolerance

  override def numOfBits: Int =
    settings.numOfBits

  override def numOfHashFunctions: Int =
    settings.numOfHashFunctions

  override def size: Int = _size

  override def falsePositiveProbability: Double =
    settings.falsePositiveProbability(_size)

  override def add(value: A): DefaultBloomFilter[A] = {
    val indices = indicesOf(value)
    if (contains(indices)) {
      this
    } else {
      val newSize = _size + 1
      val newBuffer =
        indices.foldLeft(buffer)((buffer, index) => buffer.set(index))
      new DefaultBloomFilter[A](settings, hashFunctions, newSize, newBuffer)
    }
  }

  override def apply(value: A): Boolean =
    contains(value)

  override def contains(value: A): Boolean =
    contains(indicesOf(value))

  @inline private def indicesOf(value: A): Iterable[Int] =
    hashFunctions.map(f => math.abs(f(value) % numOfBits))

  @inline private def contains(indices: IterableOnce[Int]): Boolean =
    indices.iterator.forall(buffer.contains)

}

private object DefaultBloomFilter {

  /** Creates a DefaultBloomFilter with the given settings */
  def apply[A](settings: BloomFilterSettings)(implicit
      hashFunctionFactory: HashFunctionFactory[A]
  ): DefaultBloomFilter[A] = {
    val hashFunctions =
      IndexedSeq.tabulate(settings.numOfHashFunctions) { index =>
        hashFunctionFactory(index)
      }
    new DefaultBloomFilter[A](
      settings,
      hashFunctions,
      0,
      ImmutableBitSet(settings.numOfBits)
    )
  }

}
