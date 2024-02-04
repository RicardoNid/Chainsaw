package Chainsaw

import Chainsaw.arithmetic.floating.hardfloat._
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, NoRunFirrtlCompilerAnnotation}

import scala.language.postfixOps

class SingleAddSub extends RawModule {
  val io = IO(new Bundle {
    val subOp = Input(Bool())
    val a     = Input(Bits(32.W))
    val b     = Input(Bits(32.W))
    val out   = Output(Bits(32.W))
  })

  val impl = Module(new AddRecFN(8, 23))
  impl.io.a := io.a
  impl.io.b := io.b
  io.out    := impl.io.out

  impl.io.subOp          := io.subOp // adder
  impl.io.roundingMode   := 0.U      // round to even
  impl.io.detectTininess := 0.U      // don't care
}

class SingleMul extends RawModule {
  val io = IO(new Bundle {
    val a   = Input(Bits(32.W))
    val b   = Input(Bits(32.W))
    val out = Output(Bits(32.W))
  })

  val impl = Module(new MulRecFN(8, 23))
  impl.io.a := io.a
  impl.io.b := io.b
  io.out    := impl.io.out

  impl.io.roundingMode   := 0.U // round to even
  impl.io.detectTininess := 0.U // don't care
}

object Adder32Main extends App {
  (new ChiselStage).execute(
    Array("--target-dir", "output"),
    Seq(NoRunFirrtlCompilerAnnotation, ChiselGeneratorAnnotation(() => new SingleMul))
  )
}
