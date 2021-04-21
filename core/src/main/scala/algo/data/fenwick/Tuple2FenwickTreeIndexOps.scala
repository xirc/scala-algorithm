package algo.data.fenwick

private final class Tuple2FenwickTreeIndexOps[K1, K2](
    index1: FenwickTreeIndexOps[K1],
    index2: FenwickTreeIndexOps[K2]
) extends FenwickTreeIndexOps[(K1, K2)] {

  override def zero: (K1, K2) =
    (index1.zero, index2.zero)

  override def size: (K1, K2) =
    (index1.size, index2.size)

  override def upIterator(x: (K1, K2)): Iterator[(K1, K2)] =
    for {
      x1 <- index1.upIterator(x._1)
      x2 <- index2.upIterator(x._2)
    } yield (x1, x2)

  override def downIterator(x: (K1, K2)): Iterator[(K1, K2)] =
    for {
      x1 <- index1.downIterator(x._1)
      x2 <- index2.downIterator(x._2)
    } yield (x1, x2)

  override def nextLowerBound(x: (K1, K2)): (K1, K2) =
    (index1.nextLowerBound(x._1), index2.nextLowerBound(x._2))

  override def nextUpperBound(x: (K1, K2)): (K1, K2) =
    (index1.nextUpperBound(x._1), index2.nextUpperBound(x._2))

  override def isInBoundCO(x: (K1, K2)): Boolean =
    index1.isInBoundCO(x._1) && index2.isInBoundCO(x._2)

  override def isInBoundCC(x: (K1, K2)): Boolean =
    index1.isInBoundCC(x._1) && index2.isInBoundCC(x._2)

  override def boundIterator(
      lower: (K1, K2),
      upper: (K1, K2)
  ): Iterator[((K1, K2), Long)] = {
    for {
      (size1, sign1) <- index1.boundIterator(lower._1, upper._1)
      (size2, sign2) <- index2.boundIterator(lower._2, upper._2)
    } yield {
      (size1, size2) -> (sign1 * sign2)
    }
  }

}
