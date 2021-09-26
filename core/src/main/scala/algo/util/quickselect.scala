package algo.util

import scala.annotation.tailrec
import scala.collection.mutable

trait QuickSelectSyntax {
  import scala.language.implicitConversions

  implicit final def toQuickSelectOps[A](
      values: IndexedSeq[A]
  ): QuickSelectOps[A] = {
    new QuickSelectOps(values.to(mutable.IndexedBuffer))
  }

  implicit final def toQuickSelectOps[A](
      values: mutable.IndexedBuffer[A]
  ): QuickSelectOps[A] = {
    new QuickSelectOps(values)
  }

}

object QuickSelectSyntax extends QuickSelectSyntax

final class QuickSelectOps[A](val values: mutable.IndexedBuffer[A])
    extends AnyVal {

  /** Returns the k-th value.
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   The given rank is out of range.
    * @note
    *   Time Complexity: O(N)
    * @see
    *   [[https://en.wikipedia.org/wiki/Median_of_medians Median of Medians]]
    */
  def select(rank: Int)(implicit
      ordering: Ordering[A]
  ): A = {
    if (rank < 0 || rank >= values.size)
      throw new IndexOutOfBoundsException(
        s"Index out of range [0, ${values.size}): $rank"
      )
    val index =
      select(0, values.size, rank)
        .ensuring(index => index >= 0 && index < values.size)
    values(index)
  }

  /** Returns an index at which the given rank value is located.
    */
  @tailrec
  private def select(
      from: Int,
      until: Int,
      rank: Int
  )(implicit
      ordering: Ordering[A]
  ): Int = {
    val size = until - from
    if (size == 1) {
      from
    } else {
      val pivotIndex = pivot(from, until)
      val finalPivotIndex = partition(from, until, rank, values(pivotIndex))
      if (finalPivotIndex < rank) {
        select(pivotIndex + 1, until, rank)
      } else if (finalPivotIndex > rank) {
        select(from, finalPivotIndex, rank)
      } else {
        rank
      }
    }
  }

  /** Returns an index of the median of medians */
  private def pivot(
      from: Int,
      until: Int
  )(implicit
      ordering: Ordering[A]
  ): Int = {
    val size = until - from
    if (size <= 5) {
      median5(from, until)
    } else {
      val n = (until - from + 4) / 5
      for (i <- from until until by 5) {
        val median = median5(i, math.min(i + 5, until))
        swap(median, from + (i - from) / 5)
      }
      val k = from + (n - 1) / 2
      select(from, from + n, k)
    }
  }

  /** Returns an index of the pivot considering the given rank
    *
    * Returns the given rank as the pivot index only if the pivot value is the
    * desired rank. Returns the smallest pivot index if the number of the
    * smaller elements than the pivot value is greater than the given rank.
    * Returns the greatest pivot index if the number of the non-greater elements
    * than the pivot value is less than the given rank.
    */
  @inline
  private def partition(
      from: Int,
      until: Int,
      rank: Int,
      pivotValue: A
  )(implicit
      ordering: Ordering[A]
  ): Int = {
    var fromEqualIndex = from
    for (i <- from until until) {
      if (ordering.lt(values(i), pivotValue)) {
        swap(fromEqualIndex, i)
        fromEqualIndex += 1
      }
    }
    var untilEqualIndex = fromEqualIndex
    for (i <- fromEqualIndex until until) {
      if (ordering.equiv(values(i), pivotValue)) {
        swap(untilEqualIndex, i)
        untilEqualIndex += 1
      }
    }
    if (rank < fromEqualIndex) {
      fromEqualIndex
    } else if (rank >= untilEqualIndex) {
      untilEqualIndex - 1
    } else {
      rank
    }
  }

  /** Returns an index of the median of the given range */
  @inline
  private def median5(from: Int, until: Int)(implicit
      ordering: Ordering[A]
  ): Int = {
    for (i <- from + 1 until until) {
      for (j <- i until from by -1 if ordering.lt(values(j), values(j - 1))) {
        swap(j, j - 1)
      }
    }
    from + (until - from - 1) / 2
  }

  /** Swaps the i-th element and the j-th element */
  @inline
  private def swap(i: Int, j: Int): Unit = {
    val tmp = values(i)
    values(i) = values(j)
    values(j) = tmp
  }

}
