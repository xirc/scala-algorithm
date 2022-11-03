package algo.data.bloom.mutable

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

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(bloomFilter.size === 1)

    }

    "return two if a BloomFilter contains two items" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      bloomFilter.add("b")
      assert(bloomFilter.size === 2)

    }

  }

  "BloomFilter.falsePositiveProbability" should {

    "return the false positive probability of a BloomFilter" in {

      val bloomFilter = BloomFilter[String](capacity = 10, tolerance = 0.05)
      assert(bloomFilter.numOfBits === 63)
      assert(bloomFilter.numOfHashFunctions === 5)
      assert(bloomFilter.falsePositiveProbability === 0.0)

      bloomFilter.add("a")
      assert(bloomFilter.falsePositiveProbability === 2.586e-6 +- 1e-9)

      bloomFilter.add("b")
      assert(bloomFilter.falsePositiveProbability === 6.8114e-5 +- 1e-9)

      bloomFilter.add("c")
      assert(bloomFilter.falsePositiveProbability === 4.26945e-4 +- 1e-9)

    }

  }

  "BloomFilter.add" should {

    "add the given item to a BloomFilter" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(bloomFilter.contains("a"))

    }

    "increase the size of a BloomFilter if the BloomFilter doesn't contain the item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(bloomFilter.size === 1)

    }

    "not increase the size of a BloomFilter if the BloomFilter contains the item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(bloomFilter.size === 1)

      bloomFilter.add("a")
      assert(bloomFilter.size === 1)

    }

  }

  "BloomFilter.apply" should {

    "return false if a BloomFilter does not contain the given item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(!bloomFilter("b"))

    }

    "return true if a BloomFilter contains the given item" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
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

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")
      assert(bloomFilter.contains("a"))

    }

  }

  "BloomFilter.clone" should {

    "returns a cloned BloomFilter that does not share the internal buffer of the original BloomFilter" in {

      val bloomFilter = BloomFilter[String](capacity = 100, tolerance = 0.01)
      bloomFilter.add("a")

      val clonedBloomFilter = bloomFilter.clone()
      assert(clonedBloomFilter.contains("a"))

      clonedBloomFilter.add("c")
      assert(clonedBloomFilter.contains("c"))
      assert(!bloomFilter.contains("c"))

      bloomFilter.add("b")
      assert(bloomFilter.contains("b"))
      assert(!clonedBloomFilter.contains("b"))

    }

  }

}
