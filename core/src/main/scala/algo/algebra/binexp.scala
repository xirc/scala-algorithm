package algo.algebra

import cats.Monoid

import scala.language.implicitConversions

trait BinaryExponentiationSyntax {

  implicit final def binaryExponentiationSyntax[A: BinaryExponentiation](
      a: A
  ): BinaryExponentiationOps[A] =
    new BinaryExponentiationOps(a)

  implicit final def binaryExponentiationMonoidSyntax[A](
      m: Monoid[A]
  ): BinaryExponentiationMonoidOps[A] =
    new BinaryExponentiationMonoidOps(m)

}

object BinaryExponentiationSyntax extends BinaryExponentiationSyntax

final class BinaryExponentiationOps[A](private val a: A) extends AnyVal {

  /** Compute the value of `a` raised to the power of `n`
    * @note
    *   Time Complexity: O(`log(N)`)
    * @return
    *   `a ^ n`
    */
  def binexp(n: Long)(implicit ev: BinaryExponentiation[A]): A = {
    ev.binexp(a, n)
  }

}

final class BinaryExponentiationMonoidOps[A](private val m: Monoid[A])
    extends AnyVal {

  /** Create a `BinaryExponentiation[A]` instance from the given `Monoid[A]` */
  def toBinaryExponentiation: BinaryExponentiation[A] = {
    BinaryExponentiation.fromMonoid(m)
  }

}

trait BinaryExponentiation[@specialized A] {

  def monoid: Monoid[A]

  /** Compute the value of `a` raised to the power of `n`
    * @note
    *   Time Complexity: O(`log(N)`)
    * @return
    *   `a ^ n`
    */
  def binexp(a: A, n: Long): A = {
    var ans = monoid.empty
    var p = a
    var m = n
    while (m > 0) {
      if ((m & 1) == 1) {
        ans = monoid.combine(ans, p)
      }
      p = monoid.combine(p, p)
      m >>= 1
    }
    ans
  }

}

object BinaryExponentiation {

  /** Access an implicit `BinaryExponentiation[A]` */
  @inline def apply[A](implicit
      binexp: BinaryExponentiation[A]
  ): BinaryExponentiation[A] = binexp

  /** Create a `BinaryExponentiation[A]` instance from the given function and
    * the value
    */
  @inline def instance[A](
      emptyValue: A,
      cmb: (A, A) => A
  ): BinaryExponentiation[A] =
    new BinaryExponentiation[A]() {
      override def monoid: Monoid[A] = Monoid.instance(emptyValue, cmb)
    }

  /** Create a `BinaryExponentiation[A]` instance from the given `Monoid[A]` */
  @inline def fromMonoid[A](implicit
      m: Monoid[A]
  ): BinaryExponentiation[A] =
    new BinaryExponentiation[A]() {
      override def monoid: Monoid[A] = m
    }

  /** The default instance of `BinaryExponentiation[Int]` */
  implicit val binexpIntMonoid: BinaryExponentiation[Int] =
    instance[Int](1, _ * _)

  /** The default instance of `BinaryExponentiation[Long]` */
  implicit val binexpLongMonoid: BinaryExponentiation[Long] =
    instance(1, _ * _)

  /** The default instance of `BinaryExponentiation[Float]` */
  implicit val binexpFloatMonoid: BinaryExponentiation[Float] =
    instance(1.0f, _ * _)

  /** The default instance of `BinaryExponentiation[Double]` */
  implicit val binexpDoubleMonoid: BinaryExponentiation[Double] =
    instance(1.0, _ * _)

  /** The default instance of `BinaryExponentiation[BigInt]` */
  implicit val binexpBigIntMonoid: BinaryExponentiation[BigInt] =
    instance(1, _ * _)

}
