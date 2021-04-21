package algo.data.fenwick

object TestKit {

  /** sum of [from, from+1, from+2, ..., until-2, until-1] */
  def sumOfArithmeticProgression(from: Long, until: Long): Long =
    sumOfArithmeticProgression(from, until, 1)

  /** sum of [from, from+step, from+2*step, ...] */
  def sumOfArithmeticProgression(from: Long, until: Long, step: Long): Long = {
    if (from >= until) {
      0
    } else {
      val size = (from until until by step).size
      size * (2 * from + (size - 1) * step) / 2
    }
  }

}
