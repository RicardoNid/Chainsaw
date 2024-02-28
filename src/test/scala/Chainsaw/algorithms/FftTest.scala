package Chainsaw.algorithms

import breeze.math.Complex
import org.scalatest.flatspec.AnyFlatSpecLike

class FftTest extends AnyFlatSpecLike {

  def same(a: Seq[Complex], b: Seq[Complex]) =
    a.zip(b).forall { case (complex0, complex1) => (complex0 - complex1).abs < 1e-3 }

  "fft" should "work" in {
    def testFft(N: Int, inverse: Boolean) = {
      val randComplex = (0 until N).map(_ => Complex(math.random(), math.random()))
      val transformed = GoldenFft.goldenFft(randComplex, inverse)
      val transform   = Fft.getFft(inverse, Fft.getFactorsForStreaming(N, 4))
      val yours       = transform.transform(randComplex)

      assert(
        same(transformed, yours),
        s"\ntransformed = ${transformed.mkString(",")}\nbyCooleyTukey = ${yours.mkString(",")}"
      )
    }

    (1 until 7).foreach(i => testFft(1 << i, inverse = false))
    (1 until 7).foreach(i => testFft(1 << i, inverse = true))
  }

  it should "indicate how it should be folded" in {
    val N = 32
    (1 until 7).foreach { i =>
      val parallelism = 1 << i
      val factors     = Fft.getFactorsForStreaming(N, parallelism)
      val transform   = Fft.getFft(inverse = false, factors)
      transform.printSteps()
      transform.printFolded(parallelism)
    }

  }

}
