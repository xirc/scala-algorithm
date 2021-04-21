package algo.data.fenwick

private final class LongFenwickTreeIndexOps(_size: Long)
    extends FenwickTreeIndexOps[Long] {

  def up(x: Long): Long = x | (x + 1)

  def down(x: Long): Long = (x & (x + 1)) - 1

  override def upIterator(x: Long): Iterator[Long] =
    Iterator.iterate(x)(up).takeWhile(isInBoundCO)

  override def downIterator(x: Long): Iterator[Long] =
    Iterator.iterate(x)(down).takeWhile(isInBoundCO)

  override def zero: Long = 0

  override def size: Long = _size

  override def nextUpperBound(x: Long): Long = x + 1

  override def nextLowerBound(x: Long): Long = x - 1

  override def isInBoundCO(x: Long): Boolean = x >= zero && x < size

  override def isInBoundCC(x: Long): Boolean = x >= zero && x <= size

  override def boundIterator(
      lower: Long,
      upper: Long
  ): Iterator[(Long, Long)] = {
    if (lower >= upper) {
      Iterator.empty
    } else {
      Iterator(lower -> -1, upper -> +1)
    }
  }

}
