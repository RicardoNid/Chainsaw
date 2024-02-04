package Chainsaw

import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}

object FFTExample {
  def main(args: Array[String]): Unit = {
    // Create a Fast Fourier Transformer
    val fft = new FastFourierTransformer(DftNormalization.STANDARD)
    // Create an input data array
    val data = Array(1.0, 2.0, 1.0, 2.0, 1.0, 2.0, 1.0, 2.0)
    try {
      // Perform FFT transformation
      val complexData = fft.transform(data, TransformType.FORWARD)
      for (c <- complexData) {
        System.out.println(c)
      }
    } catch {
      case e: IllegalArgumentException =>
        System.out.println(e)
    }
  }
}

object Fft {

  val fft = new FastFourierTransformer(DftNormalization.STANDARD)

  def goldenFft(input: Array[Complex])  = fft.transform(input, TransformType.FORWARD)
  def goldenIfft(input: Array[Complex]) = fft.transform(input, TransformType.INVERSE)

  def interleave(input: Array[Complex], factor: Int) = {
    input
      .grouped(factor)
      .toSeq
      .transpose
      .flatten
      .toArray
  }

  def fft2Stage(input: Array[Complex], parallel: Int) = {
    val N = input.length
    require(N % parallel == 0, s"input length $N is not divisible by factor $parallel")
    val iter = N / parallel

    val interleaved0 = interleave(input, parallel)
    val transformed0 = interleaved0.grouped(parallel).map(goldenFft).toSeq.flatten.toArray
    val interleaved1 = interleave(transformed0, iter)
    val transformed1 = interleaved1.grouped(iter).map(goldenFft).toSeq.flatten.toArray
    val interleaved2 = interleave(transformed1, parallel)
    interleaved2
  }

  def main(args: Array[String]): Unit = { // test

    def same(a: Array[Complex], b: Array[Complex]) =
      a.zip(b).forall { case (complex0, complex1) => complex0.subtract(complex1).abs() < 1e-3 }

    val randComplex = (0 until 8).map(_ => new Complex(Math.random(), Math.random())).toArray
    val transformed = goldenFft(randComplex)

    // 1. golden
    val recovered = Fft.goldenIfft(transformed)
    assert(same(randComplex, recovered))

    // 2. 2-stage
    val transformed2stage = fft2Stage(randComplex, 2)
    assert(
      same(transformed, transformed2stage),
      s"\ntransformed = ${transformed.mkString(",")}\ntransformed2stage = ${transformed2stage.mkString(",")}"
    )

  }

}
