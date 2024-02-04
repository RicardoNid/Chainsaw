package Chainsaw.arithmetic.floating

import flopoco._
import Chainsaw.edaFlow.Device.UltraScalePlus
import spinal.core._
import spinal.lib.experimental.math._

import scala.language.postfixOps

/** Floating point numeric operations implemented by Flopoco. Only works under the Linux system, depends on FloPoCo and
  * GHDL
  */
object FloatingOpsFlopoco {

  implicit class FloatingOpsFlopoco(f: Floating) {

    def assertFormat(that: Floating): Unit = {
      assert(
        f.exponentSize == that.exponentSize && f.mantissaSize == that.mantissaSize,
        "Floating point addition requires the same format"
      )
    }


    def +(that: Floating): Floating = {
      f.assertFormat(that)
      val add = FloatingAdd(f.exponentSize, f.mantissaSize, UltraScalePlus, 10 MHz)
      add.x := f
      add.y := that
      add.z
    }

    def -(that: Floating): Floating = {
      f.assertFormat(that)
      val sub = FloatingSub(f.exponentSize, f.mantissaSize, UltraScalePlus, 10 MHz)
      sub.x := f
      sub.y := that
      sub.z
    }

    def *(that: Floating): Floating = {
      f.assertFormat(that)
      val mul = FloatingMult(f.exponentSize, f.mantissaSize, UltraScalePlus, 10 MHz)
      mul.x := f
      mul.y := that
      mul.z
    }

    def /(that: Floating): Floating = {
      f.assertFormat(that)
      val div = FloatingDiv(f.exponentSize, f.mantissaSize, UltraScalePlus, 10 MHz)
      div.x := f
      div.y := that
      div.z
    }

    // TODO: constant multiplication

    def *(that: Double) = {
      val mul = FloatingConstMult(
        f.exponentSize,
        f.mantissaSize,
        f.exponentSize,
        f.mantissaSize,
        that,
        UltraScalePlus,
        10 MHz
      )
      mul.x := f
      mul.z
    }
  }

  def main(args: Array[String]): Unit = { // example

    case class FlopocoFloatingModuleExample() extends Module {
      val a, b = in(Floating32())
      val c    = out(Floating32())
      c := a + b
    }

    SpinalVerilog(FlopocoFloatingModuleExample())

  }

}
