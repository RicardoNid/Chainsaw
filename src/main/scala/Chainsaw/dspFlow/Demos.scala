package Chainsaw.dspFlow

import Chainsaw.dsp.Algos.{doCooleyTukey, getFftTwiddle}
import spinal.core.{SpinalVerilog, log2Up}

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
    case 2 => // fully-pipelined FFT
      val N       = 4
      val inputs  = (0 until N).map(i => CS(s"i$i")) // declaring complex signals
      val outputs = (0 until N).map(i => CS(s"o$i"))
      val dft2    = (data: Seq[ComplexSignal]) => Seq(data(0) + data(1), data(0) - data(1))
      val twiddle = (data: ComplexSignal, index: Int, N: Int) => data * getFftTwiddle(N, index)
      val ret =
        doCooleyTukey(
          data      = inputs,
          factors   = Seq.fill(log2Up(N))(2),
          transform = dft2,
          twiddle   = twiddle,
          inverse   = false
        )
      outputs.zip(ret).foreach { case (output, data) => output := data }
    case 3 => // fully-pipelined sorting by bitonic sort
    case 4 => // partial-parallel base-2 permutation
    case 5 => // partial-parallel FFT
    case 6 => // partial-parallel sorting by bitonic sort // TODO: showing how to use sub-graph
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

}
