package algo.data.bloom.immutable

import algo.data.bloom.{HashFunctionFactory, immutable}

import scala.annotation.nowarn

object BloomFilterExample extends App {

  locally {
    // #creating_instance
    import HashFunctionFactory.Implicits.default

    val bloomFilter: immutable.BloomFilter[String] =
      immutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    // #creating_instance
  }: @nowarn

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_parameters
    val bloomFilter =
      immutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)

    // Getting the capacity
    assert(bloomFilter.capacity == 1_000_000)

    // Getting the tolerance
    assert(bloomFilter.tolerance == 0.001)

    // Getting the number of bits
    assert(bloomFilter.numOfBits == 14_377_588)

    // Getting the number of hash functions
    assert(bloomFilter.numOfHashFunctions == 10)
    // #getting_parameters
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #adding_item
    val bloomFilter =
      immutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)

    val newBloomFilter: immutable.BloomFilter[String] =
      bloomFilter.add("item1")
    // #adding_item
  }: @nowarn

  locally {
    import HashFunctionFactory.Implicits.default
    // #testing_membership
    val bloomFilter =
      immutable
        .BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(!bloomFilter.contains("item1"))

    val newBloomFilter = bloomFilter.add("item1")
    assert(newBloomFilter.contains("item1"))
    // #testing_membership
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_size
    val bloomFilter =
      immutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(bloomFilter.size == 0)

    val newBloomFilter = bloomFilter.add("item1")
    assert(newBloomFilter.size == 1)
    // #getting_size
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_false_positive_probability
    val bloomFilter =
      immutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(bloomFilter.falsePositiveProbability == 0.0)

    val newBloomFilter = bloomFilter.add("item1")
    println(newBloomFilter.falsePositiveProbability == 2.6493427380062913e-62)
    // #getting_false_positive_probability
  }

}
