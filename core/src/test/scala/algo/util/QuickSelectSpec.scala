package algo.util

import algo.testing.BaseSpec

import scala.util.Random

final class QuickSelectSpec extends BaseSpec {
  import QuickSelectSyntax.*

  "selects k-th smallest element in small cases" in {

    val n1 = IndexedSeq(1)
    assert(n1.select(0) === 1)

    val n21 = IndexedSeq(2, 1)
    assert(n21.select(0) === 1)
    assert(n21.select(1) === 2)

    val n11 = IndexedSeq(1, 1)
    assert(n11.select(0) === 1)
    assert(n11.select(1) === 1)

    val n121 = IndexedSeq(1, 2, 1)
    assert(n121.select(0) === 1)
    assert(n121.select(1) === 1)
    assert(n121.select(2) === 2)

  }

  "selects k-th smallest element of an unsorted unique collection" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val values = random.shuffle((1 to 100).toIndexedSeq)
      for (i <- 0 until 100) {
        val expectedValue = i + 1
        assert(values.select(i) === expectedValue)
      }

    }

  }

  "selects k-th greatest element of an unsorted unique collection" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val values = random.shuffle((1 to 100).toIndexedSeq)
      for (i <- 0 until 100) {
        val expectedValue = 100 - i
        assert(values.select(i) === expectedValue)
      }

    }

  }

  "selects k-th smallest element of an unsorted non-unique collection" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed] ") {
      val random = new Random(seed)

      val values = IndexedSeq.fill(100)(random.nextInt(30))
      val sortedValues = values.sorted
      for (i <- 0 until 100) {
        val expectedValue = sortedValues(i)
        assert(values.select(i) === expectedValue)
      }

    }

  }

  "selects k-th greatest element of an unsorted non-unique collection" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse

    val seed = System.nanoTime()
    withClue(s"seed=[$seed] ") {
      val random = new Random(seed)

      val values = IndexedSeq.fill(100)(random.nextInt(30))
      val sortedValues = values.sorted
      for (i <- 0 until 100) {
        val expectedValue = sortedValues(i)
        assert(values.select(i) === expectedValue)
      }

    }

  }

  "throws an IndexOutOfRangeException if the given rank is out of range" in {

    val values = IndexedSeq.tabulate(10)(identity)

    intercept[IndexOutOfBoundsException] {
      values.select(-1)
    }

    intercept[IndexOutOfBoundsException] {
      values.select(values.size)
    }

  }

}
