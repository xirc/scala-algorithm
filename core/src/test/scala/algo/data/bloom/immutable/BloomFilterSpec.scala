package algo.data.bloom.immutable

import algo.testing.BaseSpec
import org.scalactic.Tolerance

final class BloomFilterSpec extends BaseSpec with Tolerance {

  import algo.data.bloom.HashFunctionFactory.Implicits.default

  "BloomFilter$.apply" should {

    "throw an IllegalArgumentException if the given capacity is zero" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = 0, tolerance = 0.01)
      }
      assert(
        exception.getMessage === "requirement failed: capacity [0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given capacity is negative" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = -1, tolerance = 0.01)
      }
      assert(
        exception.getMessage === "requirement failed: capacity [-1] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is zero" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = 100, tolerance = 0.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [0.0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is negative" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = 100, tolerance = -1.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [-1.0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is one" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = 100, tolerance = 1.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [1.0] must be less than one."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is greater than one" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilter[String](capacity = 100, tolerance = 2.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [2.0] must be less than one."
      )

    }

  }

  "BloomFilter.capacity" should {

    "return the capacity of a BloomFilter" in {

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.01
        ).capacity === 100
      )

      assert(
        BloomFilter[String](
          capacity = 1000,
          tolerance = 0.01
        ).capacity === 1000
      )

    }

  }

  "BloomFilter.tolerance" should {

    "return the tolerance of a BloomFilter" in {

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.01
        ).tolerance === 0.01
      )

      assert(
        BloomFilter[String](
          capacity = 1000,
          tolerance = 0.001
        ).tolerance === 0.001
      )

    }

  }

  "BloomFilter.numOfBits" should {

    "return the number of bits a BloomFilter has" in {

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.01
        ).numOfBits === 959
      )

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.001
        ).numOfBits === 1438
      )

    }

  }

  "BloomFilter.numOfHashFunctions" should {

    "return the number of hash functions a BloomFilter uses" in {

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.01
        ).numOfHashFunctions === 7
      )

      assert(
        BloomFilter[String](
          capacity = 100,
          tolerance = 0.001
        ).numOfHashFunctions === 10
      )

    }

  }

  "BloomFilter.size" should {

    "return zero if a BloomFilter contains no items" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      assert(bloomFilter.size === 0)

    }

    "return one if a BloomFilter contains one item" in {

      val bloomFilter =
        BloomFilter[String](capacity = 100, tolerance = 0.01)
          .add("a")
      assert(bloomFilter.size === 1)

    }

    "return two if a BloomFilter contains two items" in {

      val bloomFilter =
        BloomFilter[String](capacity = 100, tolerance = 0.01)
          .add("a")
          .add("b")
      assert(bloomFilter.size === 2)

    }

  }

  "BloomFilter.falsePositiveProbability" should {

    "return the false positive probability of a BloomFilter" in {

      val bloomFilter0 = BloomFilter[String](capacity = 10, tolerance = 0.05)
      assert(bloomFilter0.numOfBits === 63)
      assert(bloomFilter0.numOfHashFunctions === 5)
      assert(bloomFilter0.falsePositiveProbability === 0.0)

      val bloomFilter1 = bloomFilter0.add("a")
      assert(bloomFilter1.falsePositiveProbability === 2.586e-6 +- 1e-9)

      val bloomFilter2 = bloomFilter1.add("b")
      assert(bloomFilter2.falsePositiveProbability === 6.8114e-5 +- 1e-9)

      val bloomFilter3 = bloomFilter2.add("c")
      assert(bloomFilter3.falsePositiveProbability === 4.26945e-4 +- 1e-9)

    }

  }

  "BloomFilter.add" should {

    "add the given item to a BloomFilter" in {

      val bloomFilter0 = BloomFilter[String](capacity = 100, tolerance = 0.01)
      val bloomFilter1 = bloomFilter0.add("a")
      assert(bloomFilter1.contains("a"))

    }

    "increase the size of a BloomFilter if the BloomFilter doesn't contain the item" in {

      val bloomFilter0 = BloomFilter[String](capacity = 100, tolerance = 0.01)
      val bloomFilter1 = bloomFilter0.add("a")
      assert(bloomFilter1.size === 1)

    }

    "not increase the size of a BloomFilter if the BloomFilter contains the item" in {

      val bloomFilter0 = BloomFilter[String](capacity = 100, tolerance = 0.01)
      val bloomFilter1 = bloomFilter0.add("a")
      assert(bloomFilter1.size === 1)

      val bloomFilter2 = bloomFilter1.add("a")
      assert(bloomFilter2.size === 1)

    }

    "not increase the size of an original BloomFilter " +
      "after the original BloomFilter returns a new BloomFilter with the given item" in {

        val bloomFilter0 = BloomFilter[String](capacity = 100, tolerance = 0.01)
        bloomFilter0.add("a")
        assert(bloomFilter0.size == 0)

      }

  }

  "BloomFilter.apply" should {

    "return false if a BloomFilter does not contain the given item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(!bloomFilter("b"))

    }

    "return true if a BloomFilter contains the given item" in {

      val bloomFilter =
        BloomFilter[String](capacity = 100, tolerance = 0.01)
          .add("a")
      assert(bloomFilter("a"))

    }

  }

  "BloomFilter.contains" should {

    "return false if a BloomFilter does not contain the given item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(!bloomFilter.contains("b"))

    }

    "return true if a BloomFilter contains the given item" in {

      val bloomFilter =
        BloomFilter[String](capacity = 100, tolerance = 0.01)
          .add("a")
      assert(bloomFilter.contains("a"))

    }

  }

}
