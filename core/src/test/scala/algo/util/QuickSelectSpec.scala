package algo.util

import algo.testing.BaseSpec

import scala.util.Random

final class QuickSelectSpec extends BaseSpec {
  import QuickSelectSyntax.*

  "selects k-th smallest element in small cases" in {

    val n1 = IndexedSeq(1)
    n1.select(0) shouldBe 1

    val n21 = IndexedSeq(2, 1)
    n21.select(0) shouldBe 1
    n21.select(1) shouldBe 2

    val n11 = IndexedSeq(1, 1)
    n11.select(0) shouldBe 1
    n11.select(1) shouldBe 1

    val n121 = IndexedSeq(1, 2, 1)
    n121.select(0) shouldBe 1
    n121.select(1) shouldBe 1
    n121.select(2) shouldBe 2

  }

  "selects k-th smallest element of an unsorted unique collection" in {

    val seed = System.nanoTime()
    Random.setSeed(seed)
    withClue(s"[seed=$seed]") {
      val values = Random.shuffle((1 to 100).toIndexedSeq)
      for (i <- 0 until 100) {
        val expectedValue = i + 1
        values.select(i) shouldBe expectedValue
      }
    }

  }

  "selects k-th greatest element of an unsorted unique collection" in {

    val seed = System.nanoTime()
    Random.setSeed(seed)
    withClue(s"[seed=$seed]") {
      implicit val ordering: Ordering[Int] = Ordering.Int.reverse
      val values = Random.shuffle((1 to 100).toIndexedSeq)
      for (i <- 0 until 100) {
        val expectedValue = 100 - i
        values.select(i) shouldBe expectedValue
      }
    }

  }

  "selects k-th smallest element of an unsorted non-unique collection" in {

    val seed = System.nanoTime()
    Random.setSeed(seed)
    withClue(s"[seed=$seed] ") {
      val values = IndexedSeq.fill(100)(Random.nextInt(30))
      val sorted = values.sorted
      for (i <- 0 until 100) {
        val expectedValue = sorted(i)
        values.select(i) shouldBe expectedValue
      }
    }

  }

  "selects k-th greatest element of an unsorted non-unique collection" in {

    val seed = System.nanoTime()
    Random.setSeed(seed)
    withClue(s"[seed=$seed] ") {
      val values = IndexedSeq.fill(100)(Random.nextInt(30))
      val sorted = values.sorted
      for (i <- 0 until 100) {
        implicit val ordering: Ordering[Int] = Ordering.Int.reverse
        val expectedValue = sorted(100 - 1 - i)
        values.select(i) shouldBe expectedValue
      }
    }

  }

  "throws an IndexOutOfRangeException if the given rank is out of range" in {

    val values = IndexedSeq.tabulate(10)(identity)

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      values.select(-1)
    }

    a[IndexOutOfBoundsException] shouldBe thrownBy {
      values.select(values.size)
    }

  }

}
