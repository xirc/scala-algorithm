package algo.syntax

import algo.algebra.BinaryExponentiation
import algo.testing.BaseSpec
import cats.kernel.Monoid

final class SyntaxSpec extends BaseSpec {
  import algo.syntax.all.*

  "BinaryExponentiation" that {

    "binexp" in {

      assert(2.binexp(10) === 1024)

    }

    "toBinaryExponentiation" in {

      implicit val plus: BinaryExponentiation[Int] =
        Monoid[Int].toBinaryExponentiation
      assert(2.binexp(10) === 20)

    }

  }

  "QuickSelect" in {

    val xs = IndexedSeq(4, 3, 5, 1, 2)
    assert(xs.select(0) === 1)
    assert(xs.select(1) === 2)
    assert(xs.select(2) === 3)
    assert(xs.select(3) === 4)
    assert(xs.select(4) === 5)

  }

}
