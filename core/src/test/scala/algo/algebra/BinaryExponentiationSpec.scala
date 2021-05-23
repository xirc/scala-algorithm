package algo.algebra

import algo.testing.BaseSpec
import cats.Monoid

final class BinaryExponentiationSpec extends BaseSpec {
  import BinaryExponentiationSyntax._

  "use the default instance of BinaryExponentiation[Int]" in {

    (2: Int).binexp(10) shouldBe 1024
    (10: Int).binexp(9) shouldBe 1_000_000_000

  }

  "use the default instance of BinaryExponentiation[Long]" in {

    (2L: Long).binexp(10) shouldBe 1024
    (10L: Long).binexp(9) shouldBe 1_000_000_000

  }

  "use the default instance of BinaryExponentiation[Float]" in {

    (2f: Float).binexp(10) shouldBe 1024f
    (10f: Float).binexp(9) shouldBe 1e9f

  }

  "use the default instance of BinaryExponentiation[Double]" in {

    (2.0: Double).binexp(10) shouldBe 1024.0
    (10.0: Double).binexp(9) shouldBe 1e9

  }

  "use the default instance of BinaryExponentiation[BigInt]" in {

    BigInt(2).binexp(10) shouldBe BigInt(1024)
    BigInt(10).binexp(100) shouldBe BigInt(10).pow(100)

  }

  "use the local scope instance of BinaryExponentiation[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      BinaryExponentiation.instance(0, _ + _)
    100.binexp(100) shouldBe 10_000

  }

  "convert an instance of BinaryExponentiation[A] from the instance of Monoid[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      BinaryExponentiation.fromMonoid
    100.binexp(100) shouldBe 10_000

  }

  "convert the instance of Monoid[A] to an instance of BinaryExponentiation[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      Monoid[Int].toBinaryExponentiation
    100.binexp(100) shouldBe 10_000

  }

  "example | compute the power in modular arithmetic" in {
    val mulModFunc: (BigInt, BigInt) => BigInt =
      (x, y) => x * y % 1_000_000_009

    implicit val mulMod: BinaryExponentiation[BigInt] =
      BinaryExponentiation.instance(1, mulModFunc)

    // O(N)
    val expected =
      Iterator.fill(1_000_000)(BigInt(3)).fold(BigInt(1))(mulModFunc)

    // O(log(N))
    val actual = BigInt(3).binexp(1_000_000)

    actual shouldBe expected

  }

}
