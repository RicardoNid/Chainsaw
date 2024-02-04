package Chainsaw.dsp

import org.scalatest.flatspec.AnyFlatSpec
import breeze.math.{i, Complex => BComplex}

class AlgosTest extends AnyFlatSpec {
  def same(a: Seq[BComplex], b: Seq[BComplex]) =
    a.zip(b).forall { case (complex0, complex1) => (complex0 - complex1).abs < 1e-3 }

  val randComplex = (0 until 128).map(_ => BComplex(math.random(), math.random()))
  val transformed = Algos.goldenFft(randComplex)

  "golden fft" should "work" in {

    val recovered = Algos.goldenIfft(transformed)

    assert(same(randComplex, recovered))
  }

  "fft by recursive Cooley-Tukey" should "work" in {

    def testRecursive(N: Int) = {
      val randComplex   = (0 until N).map(_ => BComplex(math.random(), math.random()))
      val transformed   = Algos.goldenFft(randComplex)
      val byCooleyTukey = Algos.radixRFFT(randComplex, 2)

      assert(
        same(transformed, byCooleyTukey),
        s"\ntransformed = ${transformed.mkString(",")}\nbyCooleyTukey = ${byCooleyTukey.mkString(",")}"
      )

    }

    (1 until 7).foreach(i => testRecursive(1 << i))
  }

  "fft by stage-by-stage Cooley-Tukey" should "work" in {

    def testStageByStage(N: Int, parallel: Int) = {
      val randComplex    = (0 until N).map(_ => BComplex(math.random(), math.random()))
      val transformed    = Algos.goldenFft(randComplex)
      val byStageByStage = Algos.stageByStageFFT(randComplex, parallel = parallel)
      assert(same(transformed, byStageByStage), s"failed when N = $N, parallel = $parallel")
    }

    (1 until 7).foreach(i => testStageByStage(128, 1 << i))

  }

}
