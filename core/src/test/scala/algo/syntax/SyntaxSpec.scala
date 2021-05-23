package algo.syntax

import algo.algebra.BinaryExponentiation
import algo.testing.BaseSpec
import cats.kernel.Monoid

final class SyntaxSpec extends BaseSpec {

  "binexp" that {

    import algo.syntax.binexp._

    "binexp" in {

      2.binexp(10) shouldBe 1024

    }

    "toBinaryExponentiation" in {

      implicit val plus: BinaryExponentiation[Int] =
        Monoid[Int].toBinaryExponentiation
      2.binexp(10) shouldBe 20

    }

  }

}
