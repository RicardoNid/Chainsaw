package Chainsaw.arithmetic

import Chainsaw._
import Chainsaw.device.CARRY8
import Chainsaw.edaFlow.vivado.VivadoUtil
import spinal.core._

import scala.language.postfixOps

/** long fast adder based on carry selection/increment, this consumes twice as much LUTs as binary CPA, assuring a
  * O(\sqrt{w}) critical path and constant latency = 2
  *
  * @param width
  *   width of the binary adder
  * @param blockWidth
  *   width of a block
  * @see
  *   ''Nguyen, Hong Diep, Bogdan Mihai Pasca and Thomas B. Preußer. “FPGA-Specific Arithmetic Optimizations of
  *   Short-Latency Adders.” 2011 21st International Conference on Field Programmable Logic and Applications (2011):
  *   232-237.''
  */
case class CcaAdder(override val width: Int, override val blockWidth: Int)
    extends CcaAddition(width: Int, blockWidth: Int)
    with ChainsawOperatorGenerator
    with FixedLatency {

  override def latency() = 2

  override def name = s"CcaAdder${width}_$blockWidth"

  override def fmaxEstimation = 600 MHz

  override def inputTypes =
    Seq.fill(2)(NumericType.U(width)) :+ NumericType.U(1)

  override def outputTypes = Seq(NumericType.U(width + 1))

  override def impl(testCase: TestCase) = Seq(
    BigDecimal(testCase.data.map(_.toBigInt).sum)
  )

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    yours.map(_.toBigInt).equals(golden.map(_.toBigInt))

  override def testCases = Seq.fill(1000)(randomTestCase)

  override def implNaiveH = Some(new ChainsawOperatorModule(this) {
    dataOut.head := dataIn.reduce(_ + _).d(latency())
  })

  override def implH = new ChainsawOperatorModule(this) {
    // get words
    val IndexedSeq(a, b, cin)  = dataIn.map(_.asUInt())
    val (aHead, aWords) = a.subdivideIn(blockWidth bits, strict = false).splitAt(1)
    val (bHead, bWords) = b.subdivideIn(blockWidth bits, strict = false).splitAt(1)

    // first block need no selection, LUT = blockWidth, FF = blockWidth + 1
    val (c0temp, sumWord0temp) =
      (aHead.head +^ bHead.head + cin).splitAt(aHead.head.getWidth)
    val c01    = c0temp.asUInt.d()
    val sWord0 = sumWord0temp.asUInt.d(1)

    // 0 -> 1, comparators, LUT = (width - blockWidth) * 1, FF = (width - blockWidth) * 2 + (blockCount - 1) * 2
    val c0s     = aWords.zip(bWords).map { case (a, b) => (a > ~b).d() }
    val c1s     = aWords.zip(bWords).map { case (a, b) => (a >= ~b).d() }
    val aWords1 = aWords.map(_.d())
    val bWords1 = bWords.map(_.d())

    // 1 -> 2, CCC, using carry chain primitive, LUT = blockCount
    val chainCount  = (blockCount - 1).divideAndCeil(8)
    val carryChains = Seq.fill(chainCount)(CARRY8())
    // connecting carry chains one after another
    carryChains.foreach(_.CI_TOP := False)     // banned middle carry in
    carryChains.head.CI          := c01.asBool // first of all
    carryChains.prevAndNext { case (prev, next) => next.CI := prev.CO(7) }
    // drive carry chains by c0s and c1s
    carryChains.zipWithIndex.foreach { case (carryChain, i) =>
      (0 until 8).foreach { j =>
        val index = i * 8 + j
        if (index < blockCount - 1) {
          // g & p can be generated by a LUT
          carryChain.DI(j) := c0s(index) & c0s(index) // g
          carryChain.S(j)  := c1s(index) ^ c0s(index) // p
        } else {
          carryChain.DI(j) := False
          carryChain.S(j)  := False
        }
      }
    }
    val cs = c01 +: carryChains
      .map(_.CO)
      .reverse
      .reduce(_ @@ _)
      .asBools
      .take(width)
      .map(_.asUInt)
    val cout = cs(blockCount - 1)

    // 2 -> 3, RCAs, LUT = width - blockWidth, FF = width * 2 + 1
    val sWords =
      aWords1.zip(bWords1).zip(cs).map { case ((a, b), c) => (a + b + c) }
    dataOut.head := (cout
      .d() @@ (sWord0 +: sWords).map(_.d()).reverse.reduce(_ @@ _)).toAFix
  }

  override def vivadoUtilEstimation = VivadoUtil(
    lut = width * 2 + blockCount - blockWidth,
    ff  = width * 4 - blockWidth + blockCount
  )
}
