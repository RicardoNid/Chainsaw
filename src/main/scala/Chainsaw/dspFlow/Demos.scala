package Chainsaw.dspFlow

import Chainsaw.dfg.FloatingOperators._
import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math.Floating

case class DfgUnderTest(id: Int) extends DspFlow {

  val i: Signal = S()
  val o: Signal = S()

  id match {
    case 0 => // adder
      o := i + i

  }
}

object FunctionUnderTest {
  def apply(id: Int, input: Seq[Float]): Seq[Float] = {
    id match {
      case 0 => // adder
        Seq(input.head * 2)

    }
  }
}

object DfgUnderTest extends App {

  // instantiation
  val testId = 0

  DfgUnderTest(testId).exportDrawIo("dutGraph")

}
