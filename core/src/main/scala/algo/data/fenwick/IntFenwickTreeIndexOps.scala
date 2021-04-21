package algo.data.fenwick

private final class IntFenwickTreeIndexOps(_size: Int)
    extends FenwickTreeIndexOps[Int] {

  def up(x: Int): Int = x | (x + 1)

  def down(x: Int): Int = (x & (x + 1)) - 1

  override def upIterator(x: Int): Iterator[Int] =
    Iterator.iterate(x)(up).takeWhile(isInBoundCO)

  override def downIterator(x: Int): Iterator[Int] =
    Iterator.iterate(x)(down).takeWhile(isInBoundCO)

  override def zero: Int = 0

  override def size: Int = _size

  override def nextUpperBound(x: Int): Int = x + 1

  override def nextLowerBound(x: Int): Int = x - 1

  override def isInBoundCO(x: Int): Boolean = x >= zero && x < size

  override def isInBoundCC(x: Int): Boolean = x >= zero && x <= size

  override def boundIterator(
      lower: Int,
      upper: Int
  ): Iterator[(Int, Long)] = {
    if (lower >= upper) {
      Iterator.empty
    } else {
      Iterator(lower -> -1, upper -> +1)
    }
  }

}
