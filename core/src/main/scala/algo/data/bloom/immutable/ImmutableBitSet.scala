package algo.data.bloom.immutable

private final class ImmutableBitSet private (buffer: Vector[Long]) {
  import ImmutableBitSet.*

  private final val _numOfBits =
    buffer.size * LongSize

  /** The number of bits this bitset has */
  def numOfBits: Int = _numOfBits

  /** Sets the given bit to one
    *
    * @throws java.lang.IllegalArgumentException
    *   if the given bit is less than zero
    */
  def set(bit: Int): ImmutableBitSet = {
    require(bit >= 0, s"bit [$bit] must be greater than or equal to zero.")
    require(bit < _numOfBits, s"bit [$bit] must be less than [${_numOfBits}].")
    val index = indexOf(bit)
    val newValue = buffer(index) | valueOf(bit)
    val newBuffer = buffer.updated(index, newValue)
    new ImmutableBitSet(newBuffer)
  }

  /** Returns true if this bitset sets the given bit to one
    *
    * @throws java.lang.IllegalArgumentException
    *   if the given bit is less than zero
    */
  def contains(bit: Int): Boolean = {
    require(bit >= 0, s"bit [$bit] must be greater than or equal to zero.")
    val index = indexOf(bit)
    if (index >= buffer.size) {
      false
    } else {
      (buffer(index) & valueOf(bit)) != 0
    }
  }

}

private object ImmutableBitSet {

  private final val LongSize = 64 // (bits)
  private final val WordSize = 6 // 1 << 6 == 64 (bits)
  @inline private def indexOf(bit: Int): Int = bit >> WordSize
  @inline private def valueOf(bit: Int): Long = 1L << bit

  /** Creates an ImmutableBitSet
    *
    * @throws java.lang.IllegalArgumentException
    *   if the given number of bits is less than zero
    */
  def apply(numOfBits: Int): ImmutableBitSet = {
    require(
      numOfBits >= 0,
      s"numOfBits [$numOfBits] must be greater than or equal to zero."
    )
    val size =
      (numOfBits / LongSize) +
        (if (numOfBits % LongSize != 0) 1 else 0)
    val buffer = Vector.fill(size)(0L)
    new ImmutableBitSet(buffer)
  }

}
