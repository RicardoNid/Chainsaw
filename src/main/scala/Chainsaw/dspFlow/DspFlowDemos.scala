package Chainsaw.dspFlow

import spinal.core.{SpinalVerilog, log2Up}
import Chainsaw.algorithms.{Transform, Fft}

object Dft2 extends Transform[ComplexSignal] {

  override val sizeIn: Int  = 2
  override val sizeOut: Int = 2

  override def transform(dataIn: Seq[ComplexSignal]): Seq[ComplexSignal] =
    dataIn(0) + dataIn(1) :: dataIn(0) - dataIn(1) :: Nil

  override def symbol: String = "DFT2"
}

case class DfgUnderTest(id: Int) extends DspFlow {

  // describing datapath
  id match {
    case 0 =>
      val i: Signal = S("i") // TODO: get name by macro
      val o: Signal = S("o")
      o := i + i.d()
    case 1 => // butterfly
      val i0: Signal = S("i0")
      val i1: Signal = S("i1")
      val o0: Signal = S("o0")
      val o1: Signal = S("o1")
      o0 := i0 + i1
      o1 := i0 - i1
    case 2 => // unrolled FFT
      val N       = 8
      val inputs  = (0 until N).map(i => CS(s"i$i")) // declaring complex signals
      val outputs = (0 until N).map(i => CS(s"o$i"))
      val fft = Fft.getTransform[ComplexSignal](
        dft     = (size, inverse) => Dft2,
        twiddle = (data, index, N) => data * Fft.getFftTwiddle(N, index),
        inverse = false,
        factors = Seq.fill(log2Up(N))(2)
      )
      val ret = fft.transform(inputs)
      outputs.zip(ret).foreach { case (output, data) => output := data }
    case 3 => // unrolled bitonic sort
    case 4 => // streamed linear permutation(perfect shuffle, as an example)
    case 5 => // streamed FFT
    case 6 => // streamed bitonic sort // TODO: showing how to use sub-graph
    case 7 => // partial-parallel find peak
  }
}

object DfgUnderTest extends App {

  val testId = 2
  // visualization
  DfgUnderTest(testId).exportDrawIo("dutGraph")
  // verilog generation(as module)
  SpinalVerilog(new DspModule(DfgUnderTest(testId)))
  // TODO: instantiation(as Area)
  // TODO: self-test

}
