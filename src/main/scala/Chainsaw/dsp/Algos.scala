package Chainsaw.dsp

import Chainsaw._
import Chainsaw.permutation.Permutation

import breeze.linalg.DenseVector
import breeze.math.{i, Complex => BComplex}
import breeze.numerics.constants.Pi
import breeze.numerics.exp
import breeze.signal.fourierTr._
import breeze.signal.iFourierTr._
import spinal.core.log2Up

import scala.collection.mutable.ArrayBuffer
import scala.math.log
import scala.reflect.ClassTag

/** digital signal processing algorithms and utils
  */
object Algos {

  // fft
  def goldenFft(data: Seq[BComplex]): Seq[BComplex] = dvComplex1DFFT(
    new DenseVector[BComplex](data.toArray)
  ).toArray.toSeq
  def goldenIfft(data: Seq[BComplex]): Seq[BComplex] = dvComplexIFFT(
    new DenseVector[BComplex](data.toArray)
  ).toArray.toSeq

  def omega(index: Int)(implicit N: Int) = exp(-i * 2 * Pi * index / N) // \omega ^{ik}
  def beta(index: Int)(implicit N: Int)  = exp(-i * Pi * index / N)     // \omega ^{ik}

  def getFftTwiddle(N: Int, nk: Int): BComplex = breeze.numerics.exp(BComplex(0, -2 * Pi * nk / N))

  def fftTwiddle(data: BComplex, index: Int, N: Int) = data * exp(BComplex(0, -2 * Pi * index / N))

  def getIndicesBetween(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1).flatten

  def getFftTwiddlesBetween(N1: Int, N2: Int) = getIndicesBetween(N1, N2).map(getFftTwiddle(N1 * N2, _))

  // TODO: Ntt Twiddle

  /** implement FFT recursively be decomposing N into N1 and N2
    *
    * @param data
    *   input data
    * @param factors
    *   factors of N, determine how to decompose N
    * @param transform
    *   sub-transform function
    * @param twiddle
    *   twiddle function, twiddle an element according to its index and period N
    * @param inverse
    *   do inverse transform when true
    * @tparam T
    *   input type, for FFT, it is Complex, for NTT, it is Int
    * @return
    */
  def doCooleyTukey[T: ClassTag](
      data: Seq[T],
      factors: Seq[Int],
      transform: Seq[T]      => Seq[T],
      twiddle: (T, Int, Int) => T,
      inverse: Boolean = false // TODO: implement inverse
  ): Seq[T] = {

    require(factors.product == data.length, s"cannot decompose ${data.length} into ${factors.mkString(",")}")

    def build(input: Seq[T], factors: Seq[Int]): Seq[T] = {

      val N1 = factors.head
      val N2 = input.length / N1

      def doTwiddle(input: Seq[T]) =
        input.zip(getIndicesBetween(N1, N2)).map { case (t, i) => twiddle(t, i, input.length) }

      if (factors.length == 1) transform(input)
      else {

        val permutation0 = Permutation.matrixInterleave(N1, N2)
        val permutation1 = Permutation.matrixInterleave(N2, N1)

        // T -> PTMPTP
        val permuted0 = permutation0.permute(input) // permutation 0
        println(s"do P($N1, $N2)")
        val transformed0 = permuted0.grouped(N1).map(transform).toSeq // N2 blocks, length = N1
        println(s"do T($N1)")
        val multiplied = doTwiddle(transformed0.flatten) // multiplied by twiddle factors
        println(s"do M($N1, $N2)")
        val permuted1 = permutation1.permute(multiplied) // permutation 1(transpose)
        println(s"do P($N2, $N1)")
        val transformed1 = permuted1.grouped(N2).map(build(_, factors.tail)).toSeq // N1 blocks, length = N2
        val ret          = permutation0.permute(transformed1.flatten)              // permutation 2
        println(s"do P($N1, $N2)")
        ret
      }
    }
    build(data, factors)
  }

  def doStageByStage[T: ClassTag](
      data: Seq[T],
      parallel: Int,
      transform: Seq[T]      => Seq[T],
      twiddle: (T, Int, Int) => T,
      inverse: Boolean = false
  ): Seq[T] = {

    require(parallel >= 2)

    // determine factors
    val factors = ArrayBuffer[Int]()
    while (factors.product < data.length / parallel) factors += parallel
    factors += data.length / factors.product

    // construct commands by recursive decomposition
    var ret      = data
    val commands = ArrayBuffer[String]()

    def build(factors: Seq[Int]): Unit = {
      val N  = factors.product
      val N1 = factors.head
      val N2 = N / N1
      if (factors.length == 1) commands += s"T $N1"
      else {
        commands += s"P $N1 $N2"
        commands += s"T $N1"
        commands += s"M $N1 $N2"
        commands += s"P $N2 $N1"
        build(factors.tail)
        commands += s"P $N1 $N2"
      }
    }
    build(factors)

    // execute commands
    def doCommand(command: String, input: Seq[T]) = {
      command.head match {
        case 'T' =>
          val factor = command.split(" ").tail.head.toInt
          input.grouped(factor).map(transform).toSeq.flatten
        case 'M' =>
          val Seq(row, col) = command.split(" ").tail.map(_.toInt).toSeq
          val indices       = getIndicesBetween(row, col)
          input.grouped(row * col).map(_.zip(indices).map { case (t, i) => twiddle(t, i, row * col) }).toSeq.flatten
        case 'P' =>
          val Seq(row, col) = command.split(" ").tail.map(_.toInt).toSeq
          val times         = input.length / (row * col)
          val permutation   = Permutation.matrixInterleave(row, col) ⊗ times
          permutation.permute(input)
      }
    }

    println(s"\nsteps:\n${commands.mkString("\n")}")
    commands.foreach(step => ret = doCommand(step, ret))
    ret
  }

  def cooleyTukeyFFT(data: Seq[BComplex], factors: Seq[Int]): Seq[BComplex] =
    doCooleyTukey(data, factors, transform = goldenFft, twiddle = fftTwiddle)

  def radixRFFT(data: Seq[BComplex], radix: Int): Seq[BComplex] =
    cooleyTukeyFFT(data, Seq.fill((log(data.length) / log(radix)).toInt)(radix))

  def stageByStageFFT(data: Seq[BComplex], parallel: Int) =
    doStageByStage(data, parallel, transform = goldenFft, twiddle = fftTwiddle)

  // TODO: implement Cyclic Convolution by FFT
  // TODO: implement Rader DFT
  // TODO: implement Hermitian SymmetricIFFT DFT and Real-Valued FFT

  def fold[T](data: Seq[T]) = data.take(data.length / 2).zip(data.takeRight(data.length / 2))

  // transformations
  def butterflyReal(data: Seq[Double]): Seq[Double] = fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map {
    case (d, d1) => d - d1
  }
  def butterflyComplex(data: Seq[BComplex]): Seq[BComplex] =
    fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map { case (d, d1) => d - d1 }
  def swap(data: Seq[Double]) = fold(data).map { case (d, d1) => new BComplex(d, -d1) }

  // reverse transformation
  def butterflyRealR(data: Seq[Double]): Seq[Double] = fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map {
    case (d, d1) => (d - d1)
  }
  def butterflyComplexR(data: Seq[BComplex]): Seq[BComplex] =
    fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map { case (d, d1) => (d - d1) }
  def swapR(data: Seq[BComplex]): Seq[Double] = {
    val reals = data.map(_.real)
    val imags = data.map(complex => -complex.imag)
    reals ++ imags
  }

  def bitReverse(N: Int, data: Int) = BigInt(data.toBinaryString.padToLeft(log2Up(N), '0').reverse, 2).toInt

}
