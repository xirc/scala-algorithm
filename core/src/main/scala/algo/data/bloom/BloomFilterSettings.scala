package algo.data.bloom

/** Settings of BloomFilter */
private final class BloomFilterSettings private (
    _capacity: Int,
    _tolerance: Double,
    _numOfBits: Int,
    _numOfHashFunctions: Int
) {

  assert(_tolerance > 0)
  assert(_tolerance < 1)
  assert(_numOfBits > 0)
  assert(_numOfHashFunctions > 0)

  /** The capacity of a BloomFilter */
  @inline def capacity: Int = _capacity

  /** The tolerance of a BloomFilter */
  @inline def tolerance: Double = _tolerance

  /** The number of bits a BloomFilter has */
  @inline def numOfBits: Int = _numOfBits

  /** The number of hash functions a BloomFilter uses */
  @inline def numOfHashFunctions: Int = _numOfHashFunctions

  /** Returns the false positive probability when a BloomFilter contains the
    * given number of items
    */
  @inline def falsePositiveProbability(numOfItems: Int): Double = {
    require(
      numOfItems >= 0,
      s"numOfItems [$numOfItems] must be greater than or equal to zero."
    )
    math.pow(
      1 - math.exp(-numOfHashFunctions * numOfItems / numOfBits.toDouble),
      numOfHashFunctions
    )
  }

}

/** Factories of BloomFilterSettings */
private object BloomFilterSettings {

  private final val Log2: Double = math.log(2)

  /** Creates a BloomFilterSettings with the given capacity and tolerance
    *
    * @throws java.lang.IllegalArgumentException
    *   if the given capacity or tolerance is out of range
    *   - if the given capacity is less than or equal to zero
    *   - unless the given tolerance is less than one and greater than zero
    */
  def apply(
      capacity: Int,
      tolerance: Double
  ): BloomFilterSettings = {
    require(
      capacity > 0,
      s"capacity [$capacity] must be greater than zero."
    )
    require(
      tolerance > 0,
      s"tolerance [$tolerance] must be greater than zero."
    )
    require(
      tolerance < 1,
      s"tolerance [$tolerance] must be less than one."
    )

    val log2OfMaxTolerance =
      math.log(tolerance) / Log2
    val numOfBits =
      math.ceil(-capacity * log2OfMaxTolerance / Log2).toInt
    val numOfHashFunctions =
      math.ceil(-log2OfMaxTolerance).toInt

    new BloomFilterSettings(
      _capacity = capacity,
      _tolerance = tolerance,
      _numOfBits = numOfBits,
      _numOfHashFunctions = numOfHashFunctions
    )
  }

}
