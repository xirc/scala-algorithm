package algo.data.bloom.mutable

import algo.data.bloom.{HashFunctionFactory, mutable}

import scala.annotation.nowarn

object BloomFilterExample extends App {

  locally {
    // #creating_instance
    import HashFunctionFactory.Implicits.default

    val bloomFilter: mutable.BloomFilter[String] =
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    // #creating_instance
  }: @nowarn

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_parameters
    val bloomFilter =
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)

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
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)

    bloomFilter.add("item1")
    // #adding_item
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #testing_membership
    val bloomFilter =
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(!bloomFilter.contains("item1"))

    bloomFilter.add("item1")
    assert(bloomFilter.contains("item1"))
    // #testing_membership
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_size
    val bloomFilter =
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(bloomFilter.size == 0)

    bloomFilter.add("item1")
    assert(bloomFilter.size == 1)
    // #getting_size
  }

  locally {
    import HashFunctionFactory.Implicits.default
    // #getting_false_positive_probability
    val bloomFilter =
      mutable.BloomFilter[String](capacity = 1_000_000, tolerance = 0.001)
    assert(bloomFilter.falsePositiveProbability == 0.0)

    bloomFilter.add("item1")
    assert(bloomFilter.falsePositiveProbability == 2.6493427380062913e-62)
    // #getting_false_positive_probability
  }

}
