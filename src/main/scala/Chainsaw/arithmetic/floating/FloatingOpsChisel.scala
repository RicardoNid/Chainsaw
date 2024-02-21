package Chainsaw.arithmetic.floating

import Chainsaw.DataUtil
import Chainsaw.edaFlow.vivado.VivadoTask
import spinal.core._
import spinal.core.sim.{SimClockDomainHandlePimper, SpinalSimConfig}
import spinal.lib.experimental.math._

import scala.language.{existentials, postfixOps}

// TODO: make berkley hardfloat a submodule, and implement the following operations

/** Floating point numeric operations implemented by Berkley
  */
object FloatingOpsChisel {

  case class FloatingBinaryOpBlackBox(exponentSize: Int, mantissaSize: Int, moduleName: String) extends BlackBox {
    val io = new Bundle {
      val x, y = in(Bits((exponentSize + mantissaSize + 1) bits))
      val z    = out(Bits((exponentSize + mantissaSize + 1) bits))
    }
    setDefinitionName(moduleName)
    val rtlPath = s"output/FlotingPointOps_${exponentSize}_$mantissaSize.v"
    if (!new java.io.File(rtlPath).exists()) {
      println("[Chainsaw] Generating RTL for FloatingPointOps")
      hardfloat.BlackBoxGenerator.generateOps(exponentSize, mantissaSize)
    }
    addRTLPath(rtlPath)
  }

  class FloatingBinaryOp(exponentSize: Int, mantissaSize: Int, opName: String) extends Component {
    val x, y     = in(Floating(exponentSize, mantissaSize))
    val z        = out(Floating(exponentSize, mantissaSize))
    val blackBox = FloatingBinaryOpBlackBox(exponentSize, mantissaSize, s"FP${opName}_${exponentSize}_$mantissaSize")
    blackBox.io.x := x.asBits
    blackBox.io.y := y.asBits
    z.assignFromBits(blackBox.io.z)
    setDefinitionName(s"Floating${opName}Chisel_${exponentSize}_$mantissaSize")
  }

  object FloatingBinaryOp {
    def apply(exponentSize: Int, mantissaSize: Int, opName: String) =
      new FloatingBinaryOp(exponentSize, mantissaSize, opName)
  }

  implicit class FloatingOpsChisel(f: Floating) {

    def +(that: Floating): Floating = {
      val op = FloatingBinaryOp(f.exponentSize, f.mantissaSize, "Add")
      op.x := f
      op.y := that
      op.z
    }

    def -(that: Floating): Floating = {
      val op = FloatingBinaryOp(f.exponentSize, f.mantissaSize, "Sub")
      op.x := f
      op.y := that
      op.z
    }

    def *(that: Floating): Floating = {
      val op = FloatingBinaryOp(f.exponentSize, f.mantissaSize, "Mul")
      op.x := f
      op.y := that
      op.z
    }

    def *(that: Float): Floating = {
      val op = FloatingBinaryOp(f.exponentSize, f.mantissaSize, "Mul")
      op.x := f
      op.y := that
      op.z
    }

  }

  def main(args: Array[String]): Unit = {

    case class MulAddExample() extends Component {
      val x, y, z = in(Floating(8, 23))
      val ret     = out(Floating(8, 23))
      ret := (x.d() * y.d() - z.d()).d()
    }

    SpinalSimConfig().withFstWave
      .compile(MulAddExample())
      .doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        dut.x #= 1.0f
        dut.y #= 2.0f
        dut.z #= 3.0f
        dut.clockDomain.waitSampling(10)
        assert(dut.ret.toFloat == -1.0, println(dut.ret.toFloat))
      }

    VivadoTask.synthModule("MulAddExample", MulAddExample())

  }

}
