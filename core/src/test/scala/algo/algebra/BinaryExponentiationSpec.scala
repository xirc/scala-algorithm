package algo.algebra

import algo.testing.BaseSpec
import cats.Monoid

final class BinaryExponentiationSpec extends BaseSpec {
  import BinaryExponentiationSyntax._

  "use the default instance of BinaryExponentiation[Int]" in {

    assert((2: Int).binexp(10) === 1024)
    assert((10: Int).binexp(9) === 1_000_000_000)

  }

  "use the default instance of BinaryExponentiation[Long]" in {

    assert((2L: Long).binexp(10) === 1024L)
    assert((10L: Long).binexp(9) === 1_000_000_000L)

  }

  "use the default instance of BinaryExponentiation[Float]" in {

    assert((2f: Float).binexp(10) === 1024f)
    assert((10f: Float).binexp(9) === 1e9f)

  }

  "use the default instance of BinaryExponentiation[Double]" in {

    assert((2.0: Double).binexp(10) === 1024.0)
    assert((10.0: Double).binexp(9) === 1e9)

  }

  "use the default instance of BinaryExponentiation[BigInt]" in {

    assert(BigInt(2).binexp(10) === BigInt(1024))
    assert(BigInt(10).binexp(100) === BigInt(10).pow(100))

  }

  "use the local scope instance of BinaryExponentiation[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      BinaryExponentiation.instance(0, _ + _)
    assert(100.binexp(100) === 10_000)

  }

  "convert an instance of BinaryExponentiation[A] from the instance of Monoid[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      BinaryExponentiation.fromMonoid
    assert(100.binexp(100) === 10_000)

  }

  "convert the instance of Monoid[A] to an instance of BinaryExponentiation[A]" in {

    implicit val plus: BinaryExponentiation[Int] =
      Monoid[Int].toBinaryExponentiation
    assert(100.binexp(100) === 10_000)

  }

  "example | compute the power in modular arithmetic" in {
    val mulModFunc: (BigInt, BigInt) => BigInt =
      (x, y) => x * y % 1_000_000_009

    implicit val mulMod: BinaryExponentiation[BigInt] =
      BinaryExponentiation.instance(1, mulModFunc)

    // O(N)
    val expected =
      Iterator.fill(1_000_000)(BigInt(3)).fold(BigInt(1))(mulModFunc)

    assert(BigInt(3).binexp(1_000_000) === expected)

  }

}
