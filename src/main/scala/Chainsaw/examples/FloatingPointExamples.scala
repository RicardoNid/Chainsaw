package Chainsaw.examples

import spinal.core._
import spinal.core.sim.{SimClockDomainHandlePimper, SpinalSimConfig}
import spinal.lib.experimental.math._ // for type "Floating" and ""

object FloatingPointExamples {

  case class FpAddByFlopoco() extends Module {
    val a, b = in(Floating(8, 23))
    val c    = out(Floating(8, 23))
    import Chainsaw.arithmetic.floating.FloatingOpsFlopoco._ // import floating point operations from FloPoCo, need FloPoCo executable in $PATH
    c := a + b
  }

  case class FpAddByChisel() extends Module {
    val a, b, c = in(Floating(8, 23))
    val d       = out(Floating(8, 23))
    import Chainsaw.arithmetic.floating.FloatingOpsChisel._ // import floating point operations from HardFloat
    d := (a + b) * c
  }

  // TODO: example of RecFloating operations

  def main(args: Array[String]): Unit = {
    import Chainsaw.arithmetic.floating._ // for pimper

    SpinalVerilog(FpAddByChisel())
    if (Chainsaw.FLOPOCO.exist()) SpinalVerilog(FpAddByFlopoco())

    SpinalSimConfig().withFstWave.doSim(FpAddByChisel()) { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      dut.a #= 1.0f
      dut.b #= 2.0f
      dut.c #= 3.0f
      dut.clockDomain.waitSampling(10)
      assert(dut.d.toFloat == 9.0, dut.d.toFloat)
    }

  }

}
