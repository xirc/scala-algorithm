package algo.data.bloom

import algo.testing.BaseSpec

final class HashFunctionFactorySpec extends BaseSpec {

  "HashFunctionFactory.Implicits.default.apply" should {

    "throw an IllegalArgumentException if the given index is negative" in {

      val factory = HashFunctionFactory.Implicits.default[String]
      val exception = intercept[IllegalArgumentException] {
        factory(-1)
      }
      assert(
        exception.getMessage === "requirement failed: index [-1] must be greater than or equal to zero."
      )

    }

    "create hash functions that return different hash values for the same given value" in {

      val factory = HashFunctionFactory.Implicits.default[String]
      val hashing0 = factory(0)
      val hashing1 = factory(1)
      assert(hashing0("a") !== hashing1("a"))

    }

  }

}
