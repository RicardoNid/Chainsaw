package Chainsaw.algorithms

/** compare and swap module, a basic building block of sorting networks
  */
case class Cas[T](cmp: (T, T) => Int, ascending: Boolean) extends Transform[T] {
  override val sizeIn: Int  = 2
  override val sizeOut: Int = 2

  override def transform(dataIn: Seq[T]): Seq[T] = {
    require(dataIn.size == 2, "size mismatch")
    val Seq(a, b) = dataIn
    val ret       = if (cmp(a, b) > 0) Seq(b, a) else dataIn // ascending
    if (ascending) ret else ret.reverse
  }

  override def toString: String = if (ascending) "↑" else "↓"
}

/** Bitonic sorting network implementation by transform
  */
object Bitonic {

  /** get the bitonic merge module, which make bitonic sequence sorted
    */
  def getBM[T](size: Int, ascending: Boolean, cmp: (T, T) => Int): TransformList[T] = {
    if (size == 2) Cas(cmp, ascending)
    else {
      val step0 = Cas(cmp, ascending) ⊗ (size / 2)
      val step1 = SP[T](size, 2) * (SP[T](size / 2, size / 4) ⊗ 2)
      val step2 = getBM(size / 2, ascending, cmp) ++ getBM(size / 2, ascending, cmp)
      step0 * step1 * step2
    }
  }

  /** get the bitonic sorting network
    * @param cmp
    *   a comparator, which returns a negative integer, zero, or a positive integer as the first argument is less than,
    *   equal to, or greater than the second
    */
  def getNetwork[T](size: Int, ascending: Boolean, cmp: (T, T) => Int): TransformList[T] = {
    if (size == 2) getBM(size, ascending, cmp)
    else {
      val step0 = (getNetwork(size / 2, ascending, cmp) ++ getNetwork(size / 2, !ascending, cmp))
      val step1 = SP[T](size, size / 2)
      val step2 = getBM(size, ascending, cmp)
      step0 * step1 * step2
    }
  }
}

// TODO: implement more sorting/permutation networks