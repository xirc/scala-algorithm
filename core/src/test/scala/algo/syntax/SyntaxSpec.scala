package algo.syntax

import algo.algebra.BinaryExponentiation
import algo.testing.BaseSpec
import cats.kernel.Monoid

final class SyntaxSpec extends BaseSpec {
  import algo.syntax.all.*

  "BinaryExponentiation" that {

    "binexp" in {

      2.binexp(10) shouldBe 1024

    }

    "toBinaryExponentiation" in {

      implicit val plus: BinaryExponentiation[Int] =
        Monoid[Int].toBinaryExponentiation
      2.binexp(10) shouldBe 20

    }

  }

  "QuickSelect" in {

    val xs = IndexedSeq(4, 3, 5, 1, 2)
    xs.select(0) shouldBe 1
    xs.select(1) shouldBe 2
    xs.select(2) shouldBe 3
    xs.select(3) shouldBe 4
    xs.select(4) shouldBe 5

  }

}
