package Chainsaw.algorithms

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.numerics.constants.Pi
import breeze.numerics.exp
import breeze.signal.fourierTr.dvComplex1DFFT
import breeze.signal.iFourierTr.dvComplexIFFT

import scala.collection.mutable.ArrayBuffer
import scala.math.log

object GoldenFft {
  def goldenFft(data: Seq[Complex], inverse: Boolean): Seq[Complex] =
    if (!inverse) dvComplex1DFFT(new DenseVector[Complex](data.toArray)).toArray.toSeq
    else dvComplexIFFT(new DenseVector[Complex](data.toArray)).toArray.toSeq
}

case class Dft(size: Int, inverse: Boolean) extends Transform[Complex] {
  override val sizeIn: Int  = size
  override val sizeOut: Int = size
  override def transform(dataIn: Seq[Complex]): Seq[Complex] = {
    size match {
      case 2 =>
        if (!inverse) Seq(dataIn(0) + dataIn(1), dataIn(0) - dataIn(1))
        else Seq((dataIn(0) + dataIn(1)) / 2, (dataIn(0) - dataIn(1)) / 2)
      // TODO: DFT4 and DFT8
      case _ =>
//        println(s"need more decomposition for dft-$size")
        GoldenFft.goldenFft(dataIn, inverse)
    }
  }

  override def toString: String = s"DFT($size)"
}

case class Twiddle[T](N1: Int, N2: Int, twiddle: (T, Int, Int) => T, inverse: Boolean) extends Transform[T] {

  val size                  = N1 * N2
  override val sizeIn: Int  = size
  override val sizeOut: Int = size

  override def transform(dataIn: Seq[T]): Seq[T] = {
    require(dataIn.size == size, s"size mismatch, expected $size, got ${dataIn.size}")
    val indices = Seq.tabulate(N2, N1)((n2, n1) => n2 * n1 * (if (inverse) -1 else 1)).flatten
    dataIn.zip(indices).map { case (t, i) => twiddle(t, i, size) }
  }

  override def toString: String = s"Twiddle($N1, $N2)"
}
object Fft {

  /** type-generic FFT implementation, for FFT in complex domain, NTT in integer domain, etc.
    * @param dft
    *   (size, inverse) => Transform, define dft at small size like 2, 4, 8
    * @param twiddle
    *   (data, index, size) => twiddled data, define twiddle factor generation & multiplication implementation
    * @param inverse
    *   fft/ifft
    * @param factors
    *   decomposition of N, defined how Cooley-Tukey is done
    * @return
    */
  def getTransform[T](
      dft: (Int, Boolean)    => Transform[T],
      twiddle: (T, Int, Int) => T,
      inverse: Boolean,
      factors: Seq[Int]
  ): TransformList[T] = {
    val N  = factors.product
    val N1 = factors.head
    val N2 = N / N1
    if (factors.length == 1) dft(N, inverse)
    else {
      // T -> PTMPTP
      val p0 = SP[T](N, N2)
      val t0 = dft(N1, inverse) ⊗ N2
      val m  = Twiddle(N1, N2, twiddle, inverse)
      val p1 = SP[T](N, N1)
      val t1 = getTransform(dft, twiddle, inverse, factors.tail) ⊗ N1

      p0 * t0 * m * p1 * t1 * p0
    }
  }

  def getFactorsForUnrolled(size: Int): Seq[Int] = {
    Seq.fill((log(size) / log(2)).toInt)(2)
  }

  def getFactorsForStreaming(size: Int, parallelism: Int): Seq[Int] = {
    val factors = ArrayBuffer[Int]()
    while (factors.product < size / parallelism) factors += parallelism
    factors += size / factors.product
    factors
  }

  def getFftTwiddle(N: Int, index: Int): Complex = exp(Complex(0, -2 * Pi * index / N))

  def getFft(inverse: Boolean, factors: Seq[Int]): TransformList[Complex] = {
    val dft     = Dft(_, _)
    val twiddle = (t: Complex, i: Int, N: Int) => t * exp(Complex(0, -2 * Pi * i / N))
    getTransform(dft, twiddle, inverse, factors)
  }

  // TODO: implement NTT

}
