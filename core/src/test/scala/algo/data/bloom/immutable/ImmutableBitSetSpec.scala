package algo.data.bloom.immutable

import algo.testing.BaseSpec

final class ImmutableBitSetSpec extends BaseSpec {

  "ImmutableBitSet$.apply" should {

    "throw an IllegalArgumentException if the given number of bits is negative" in {

      val exception = intercept[IllegalArgumentException] {
        ImmutableBitSet(numOfBits = -1)
      }
      assert(
        exception.getMessage === "requirement failed: numOfBits [-1] must be greater than or equal to zero."
      )

    }

    "create an ImmutableBitSet that has 0 bits if the given number of bits is zero" in {

      val bitset = ImmutableBitSet(numOfBits = 0)
      assert(bitset.numOfBits === 0)

    }

    "create an ImmutableBitSet that has 64 bits if the given number of bits is one" in {

      val bitset = ImmutableBitSet(numOfBits = 1)
      assert(bitset.numOfBits === 64)

    }

    "create an ImmutableBitSet that has 64 bits if the given number of bits is 64" in {

      val bitset = ImmutableBitSet(numOfBits = 64)
      assert(bitset.numOfBits === 64)

    }

    "create an ImmutableBitSet that has 128 bits if the given number of bits is 65" in {

      val bitset = ImmutableBitSet(numOfBits = 65)
      assert(bitset.numOfBits === 128)

    }

  }

  "ImmutableBitSet.numOfBits" should {

    "return the number of bits ImmutableBitSet has" in {

      val bitset = ImmutableBitSet(numOfBits = 64)
      assert(bitset.numOfBits === 64)

    }

  }

  "ImmutableBitSet.set" should {

    "throw an IllegalArgumentException if the given bit is negative" in {

      val bitset = ImmutableBitSet(64 * 3)
      val exception = intercept[IllegalArgumentException] {
        bitset.set(-1)
      }
      assert(
        exception.getMessage === "requirement failed: bit [-1] must be greater than or equal to zero."
      )

    }

    "throw an IllegalArgumentException if the given bit is equal to the number of bits" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      val exception = intercept[IllegalArgumentException] {
        bitset.set(numOfBits)
      }
      assert(
        exception.getMessage === s"requirement failed: bit [$numOfBits] must be less than [$numOfBits]."
      )

    }

    "throw an IllegalArgumentException if the given bit is greater than the number of bits" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      val exception = intercept[IllegalArgumentException] {
        bitset.set(numOfBits + 1)
      }
      assert(
        exception.getMessage === s"requirement failed: bit [${numOfBits + 1}] must be less than [$numOfBits]."
      )

    }

    "set the given bit to one" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      assert(bitset.set(0).contains(0))
      assert(bitset.set(50).contains(50))
      assert(bitset.set(100).contains(100))
      assert(bitset.set(150).contains(150))
      assert(bitset.set(numOfBits - 1).contains(numOfBits - 1))

    }

  }

  "ImmutableBitSet.contains" should {

    "throw an IllegalArgumentException if the given bit is negative" in {

      val bitset = ImmutableBitSet(64 * 3)
      val exception = intercept[IllegalArgumentException] {
        bitset.contains(-1)
      }
      assert(
        exception.getMessage === "requirement failed: bit [-1] must be greater than or equal to zero."
      )

    }

    "return true if an ImmutableBitSet set the given bit to one" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      assert(bitset.set(0).contains(0))
      assert(bitset.set(50).contains(50))
      assert(bitset.set(100).contains(100))
      assert(bitset.set(150).contains(150))
      assert(bitset.set(numOfBits - 1).contains(numOfBits - 1))

    }

    "return false if an ImmutableBitSet doesn't set the given bit to one" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      assert(!bitset.contains(0))
      assert(!bitset.contains(50))
      assert(!bitset.contains(100))
      assert(!bitset.contains(150))
      assert(!bitset.contains(numOfBits - 1))

    }

    "return false if the given bit is equal to the number of bits" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      assert(!bitset.contains(numOfBits))

    }

    "return false if the given bit is greater than the number of bits" in {

      val numOfBits = 64 * 3
      val bitset = ImmutableBitSet(numOfBits)
      assert(!bitset.contains(numOfBits + 1))

    }

  }

}
