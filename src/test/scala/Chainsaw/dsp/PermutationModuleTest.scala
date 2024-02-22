package Chainsaw.dsp

import Chainsaw.algorithms.LinearPermutation
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

class PermutationModuleTest extends AnyFlatSpec {

  def testPermutationModule(pm: => PermutationModule[UInt]) = {
    SpinalSimConfig().withFstWave
      .compile(pm)
      .doSim { dut =>
        dut.clockDomain.forkStimulus(2)
        dut.dataIn.valid #= false
        dut.clockDomain.waitSampling()
        val period   = dut.period
        val parallel = dut.parallel
        val latency  = dut.latency
        (0 until period * 2).foreach { i =>
          dut.dataIn.payload.zipWithIndex.foreach { case (in, j) => in #= (i % period) * parallel + j }
          dut.dataIn.valid #= true
          dut.clockDomain.waitSampling()
          if (i >= period) println(dut.dataOut.payload.map(_.toInt).mkString(" "))
//          println(dut.dataOut.map(_.toInt).mkString(" "))
        }
      }
  }

  "PermutationModule" should "work for linear permutation using RAMs" in {
    val permuted = LinearPermutation.stridePermutation(6, 2).permuted
    val parallel = 8
//    val permuted = LinearPermutation.stridePermutation(3, 2).permuted
//    val parallel = 2
    println(permuted.mkString(" "))
    testPermutationModule(new PermutationModule(permuted, parallel, HardType(UInt(6 bits))))
  }

  it should "work for unrolled permutation" in {
    val permuted = LinearPermutation.stridePermutation(3, 2).permuted
    val parallel = 1 << 3
    testPermutationModule(new PermutationModule(permuted, parallel, HardType(UInt(3 bits))))
  }

}
