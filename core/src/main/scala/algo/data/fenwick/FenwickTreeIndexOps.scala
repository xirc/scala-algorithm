package algo.data.fenwick

private trait FenwickTreeIndexOps[K] {

  def zero: K

  def size: K

  def upIterator(x: K): Iterator[K]

  def downIterator(x: K): Iterator[K]

  def nextLowerBound(x: K): K

  def nextUpperBound(x: K): K

  def isInBoundCO(x: K): Boolean

  final def outOfBoundCO(x: K): Boolean =
    !isInBoundCO(x)

  def isInBoundCC(x: K): Boolean

  final def outOfBoundCC(x: K): Boolean =
    !isInBoundCC(x)

  def boundIterator(
      lower: K,
      upper: K
  ): Iterator[(K, Long)]

}
