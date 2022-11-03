package algo.data.bloom

import scala.util.hashing.{Hashing, MurmurHash3}

/** Creates hash functions for a BloomFilter */
trait HashFunctionFactory[A] {

  /** Creates a hash function associated with the given index
    *
    * @param index
    *   the index associated with a hash function
    * @throws java.lang.IllegalArgumentException
    *   if the given index is less than zero
    */
  def apply(index: Int): A => Int

}

/** Factories of HashFunctionFactory */
object HashFunctionFactory {

  object Implicits {

    /** Creates the default HashFunctionFactory */
    implicit def default[A]: HashFunctionFactory[A] =
      new DefaultHashFunctionFactory$2[A](
        Hashing.fromFunction(x => x.toString.hashCode),
        Hashing.fromFunction(x => MurmurHash3.stringHash(x.toString))
      )

  }

  private final class DefaultHashFunctionFactory$2[A](
      hashing0: Hashing[A],
      hashing1: Hashing[A]
  ) extends HashFunctionFactory[A] {
    def apply(index: Int): A => Int = {
      require(
        index >= 0,
        s"index [$index] must be greater than or equal to zero."
      )
      (value: A) => {
        val h0 = hashing0.hash(value)
        val h1 = hashing1.hash(value)
        h0 + index * h1 + index * index
      }
    }
  }

}
