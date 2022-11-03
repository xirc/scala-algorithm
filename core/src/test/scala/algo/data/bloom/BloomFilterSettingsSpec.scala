package algo.data.bloom

import algo.testing.BaseSpec
import org.scalactic.Tolerance

final class BloomFilterSettingsSpec extends BaseSpec with Tolerance {

  "BloomFilterSettings$.apply" should {

    "throw an IllegalArgumentException if the given capacity is zero" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = 0, tolerance = 0.01)
      }
      assert(
        exception.getMessage === "requirement failed: capacity [0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given capacity is negative" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = -1, tolerance = 0.01)
      }
      assert(
        exception.getMessage === "requirement failed: capacity [-1] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is negative" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = 100, tolerance = -1.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [-1.0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is zero" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = 100, tolerance = 0.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [0.0] must be greater than zero."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is one" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = 100, tolerance = 1.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [1.0] must be less than one."
      )

    }

    "throw an IllegalArgumentException if the given tolerance is greater than one" in {

      val exception = intercept[IllegalArgumentException] {
        BloomFilterSettings(capacity = 100, tolerance = 2.0)
      }
      assert(
        exception.getMessage === "requirement failed: tolerance [2.0] must be less than one."
      )

    }

  }

  "BloomFilterSettings.capacity" should {

    "return the capacity of a BloomFilter" in {

      assert(
        BloomFilterSettings(
          capacity = 1,
          tolerance = 0.01
        ).capacity === 1
      )

      assert(
        BloomFilterSettings(
          capacity = 10,
          tolerance = 0.01
        ).capacity === 10
      )

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.01
        ).capacity === 100
      )

    }

  }

  "BloomFilterSettings.tolerance" should {

    "return the tolerance of a BloomFilter" in {

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.1
        ).tolerance === 0.1
      )

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.01
        ).tolerance === 0.01
      )

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.001
        ).tolerance === 0.001
      )

    }

  }

  "BloomFilterSettings.numOfBits" should {

    "return the number of bits a BloomFilter has" in {

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.01
        ).numOfBits === 959
      )

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.001
        ).numOfBits === 1438
      )

      assert(
        BloomFilterSettings(
          capacity = 1000,
          tolerance = 0.01
        ).numOfBits === 9586
      )

    }

  }

  "BloomFilterSettings.numOfHashFunctions" should {

    "return the number of hash functions a BloomFilter uses" in {

      assert(
        BloomFilterSettings(
          capacity = 100,
          tolerance = 0.01
        ).numOfHashFunctions === 7
      )

      assert(
        BloomFilterSettings(
          capacity = 10,
          tolerance = 0.001
        ).numOfHashFunctions === 10
      )

      assert(
        BloomFilterSettings(
          capacity = 1000,
          tolerance = 0.0001
        ).numOfHashFunctions === 14
      )

    }

  }

  "BloomFilterSettings.falsePositiveProbability" should {

    "throw an IllegalArgumentException if the given number of items is negative" in {

      val settings = BloomFilterSettings(capacity = 100, tolerance = 0.01)
      val exception = intercept[IllegalArgumentException] {
        settings.falsePositiveProbability(-1)
      }
      assert(
        exception.getMessage === "requirement failed: numOfItems [-1] must be greater than or equal to zero."
      )

    }

    "return the false positive probability when a BloomFilter contains the given number of items" in {

      val settings = BloomFilterSettings(capacity = 10, tolerance = 0.05)
      assert(settings.numOfBits === 63)
      assert(settings.numOfHashFunctions === 5)
      assert(settings.falsePositiveProbability(0) === 0.0)
      assert(settings.falsePositiveProbability(1) === 2.586e-6 +- 1e-9)
      assert(settings.falsePositiveProbability(2) === 6.8114e-5 +- 1e-9)
      assert(settings.falsePositiveProbability(3) === 4.26945e-4 +- 1e-9)
      assert(settings.falsePositiveProbability(4) === 1.488949e-3 +- 1e-9)
      assert(settings.falsePositiveProbability(10) === 4.9333977e-2 +- 1e-9)
      assert(settings.falsePositiveProbability(11) === 6.6953215e-2 +- 1e-9)

    }

  }

}
