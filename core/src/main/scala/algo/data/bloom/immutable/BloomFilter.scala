package algo.data.bloom.immutable

import algo.data.bloom.{BloomFilterSettings, HashFunctionFactory}

trait BloomFilter[A] {

  /** The capacity of this BloomFilter */
  def capacity: Int

  /** The tolerance of this BloomFilter */
  def tolerance: Double

  /** The number of bits this BloomFilter has */
  def numOfBits: Int

  /** The number of hash functions this BloomFilter uses */
  def numOfHashFunctions: Int

  /** The number of items this BloomFilter contains */
  def size: Int

  /** The false positive probability of this BloomFilter */
  def falsePositiveProbability: Double

  /** Returns a new BloomFilter that added the given item */
  def add(item: A): BloomFilter[A]

  /** Returns true if this BloomFilter contains the given item */
  def apply(item: A): Boolean

  /** Returns true if this BloomFilter contains the given item */
  def contains(item: A): Boolean

}

/** Factories of BloomFilter */
object BloomFilter {

  /** Creates a BloomFilter with the given capacity and tolerance
    *
    * @throws java.lang.IllegalArgumentException
    *   if the given capacity or tolerance is out of range
    *   - if the given capacity is less than or equal to zero
    *   - unless the given tolerance is greater than zero and less than one
    */
  def apply[A](
      capacity: Int,
      tolerance: Double
  )(implicit
      hashFunctionFactory: HashFunctionFactory[A]
  ): BloomFilter[A] = {
    val settings = BloomFilterSettings(capacity, tolerance)
    DefaultBloomFilter(settings)
  }

}
