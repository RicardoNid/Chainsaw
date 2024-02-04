package Chainsaw.arithmetic.floating.hardfloat

import chisel3.{RawModule, _}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util.MuxLookup

/** generate series of floating point operators by hard-float for specific precision
  */
object BlackBoxGenerator {

  class FpBinary(exponentSize: Int, mantissaSize: Int, opName: String) extends RawModule {
    val io = IO(new Bundle {
      val x, y = Input(Bits((exponentSize + mantissaSize + 1).W))
      val z    = Output(Bits((exponentSize + mantissaSize + 1).W))
    })

    override def desiredName: String = s"FP${opName}_${exponentSize}_${mantissaSize}"
  }

  class FpMul(exponentSize: Int, mantissaSize: Int) extends FpBinary(exponentSize, mantissaSize, "Mul") {

    val impl = Module(new MulRecFN(exponentSize, mantissaSize + 1))
    impl.io.a := recFNFromFN(exponentSize, mantissaSize + 1, io.x)
    impl.io.b := recFNFromFN(exponentSize, mantissaSize + 1, io.y)
    io.z      := fNFromRecFN(exponentSize, mantissaSize + 1, impl.io.out)

    impl.io.roundingMode   := 0.U // round to even
    impl.io.detectTininess := 0.U // don't care

  }

  class FpAdd(exponentSize: Int, mantissaSize: Int) extends FpBinary(exponentSize, mantissaSize, "Add") {

    val impl = Module(new AddRecFN(exponentSize, mantissaSize + 1))
    impl.io.a := recFNFromFN(exponentSize, mantissaSize + 1, io.x)
    impl.io.b := recFNFromFN(exponentSize, mantissaSize + 1, io.y)
    io.z      := fNFromRecFN(exponentSize, mantissaSize + 1, impl.io.out)

    impl.io.subOp          := false.B // adder
    impl.io.roundingMode   := 0.U     // round to even
    impl.io.detectTininess := 0.U     // don't care
  }

  class FpSub(exponentSize: Int, mantissaSize: Int) extends FpBinary(exponentSize, mantissaSize, "Sub") {

    val impl = Module(new AddRecFN(exponentSize, mantissaSize + 1))
    impl.io.a := recFNFromFN(exponentSize, mantissaSize + 1, io.x)
    impl.io.b := recFNFromFN(exponentSize, mantissaSize + 1, io.y)
    io.z      := fNFromRecFN(exponentSize, mantissaSize + 1, impl.io.out)

    impl.io.subOp          := true.B // subtractor
    impl.io.roundingMode   := 0.U    // round to even
    impl.io.detectTininess := 0.U    // don't care
  }

  /** this module should contains all floating point operators we need, thus, by generating .v file for this module, we
    * can get all floating point operators
    * @param exponentSize
    *   exponent size according to IEEE754
    * @param mantissaSize
    *   mantissa size according to IEEE754
    */
  class FloatingOps(exponentSize: Int, mantissaSize: Int) extends RawModule {

    val io = IO(new Bundle {
      val x, y = Input(Bits((exponentSize + mantissaSize + 1).W))
      val z    = Output(Bits((exponentSize + mantissaSize + 1).W))
      val mode = Input(UInt(1.W))
    })

    val add = Module(new FpAdd(exponentSize, mantissaSize))
    add.io.x := io.x
    add.io.y := io.y

    val sub = Module(new FpSub(exponentSize, mantissaSize))
    sub.io.x := io.x
    sub.io.y := io.y

    val mul = Module(new FpMul(exponentSize, mantissaSize))
    mul.io.x := io.x
    mul.io.y := io.y

    io.z := MuxLookup(
      io.mode,
      false.B,
      Array(
        (0.U) -> add.io.z,
        (1.U) -> sub.io.z,
        (2.U) -> mul.io.z,
        (3.U) -> mul.io.z
      )
    )

    override def desiredName: String = s"FlotingPointOps_${exponentSize}_${mantissaSize}"
  }

  /** generate all floating point operators for specific precision
    */
  def generateOps(exponentSize: Int, mantissaSize: Int) = {
    (new ChiselStage).execute(
      Array("--target-dir", "output"),
      Seq(
        ChiselGeneratorAnnotation(() => new FloatingOps(exponentSize, mantissaSize))
      )
    )
  }
}
