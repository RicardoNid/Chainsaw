package Chainsaw.dsp

import Chainsaw.algorithms.LinearPermutation
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class PermutationModuleTest extends AnyFlatSpec {

  def testPermutationModule(pm: => PermutationModule[UInt]) = {
    SpinalSimConfig().withFstWave
      .compile(pm)
      .doSim { dut =>
        dut.clockDomain.forkStimulus(2)
        dut.dataIn.valid #= false
        dut.dataOut.ready #= false
        dut.clockDomain.waitSampling()
        dut.dataOut.ready #= true // free run
        val period   = dut.period
        val parallel = dut.parallel
        val latency  = dut.latency
        val golden   = dut.permutation.permuted
        val yours    = ArrayBuffer[Int]()

        def poke() = fork {
          var i = 0
          while (true) {
            dut.dataIn.payload.zipWithIndex.foreach { case (in, j) => in #= (i % period) * parallel + j }
            if (i < period) dut.dataIn.valid #= true
            dut.clockDomain.waitSampling()
            i += 1
          }
        }

        def peek() = fork {
          while (true) {
            if (dut.dataOut.valid.toBoolean) dut.dataOut.payload.foreach(bit => yours += bit.toInt)
            dut.clockDomain.waitSampling()
          }
        }

        def wait() = dut.clockDomain.waitSampling(period + latency + 10)

        poke()
        peek()
        wait()

        assert(golden.zip(yours).forall { case (g, y) => g == y })
        println("PASS")
      }
  }

  "PermutationModule" should "work for linear permutation using RAMs" in {
    // stride permutation
    val permuted = LinearPermutation.stridePermutation(6, 2).permuted
    (2 until 6).foreach(s => testPermutationModule(new PermutationModule(permuted, 1 << s, HardType(UInt(6 bits)))))
    // bit reversal
    val reversed = LinearPermutation.bitReversal(6).permuted
    (2 until 6).foreach(s => testPermutationModule(new PermutationModule(reversed, 1 << s, HardType(UInt(6 bits)))))
  }

  it should "work for unrolled permutation" in {
    val permuted = LinearPermutation.stridePermutation(3, 2).permuted
    val parallel = 1 << 3
    testPermutationModule(new PermutationModule(permuted, parallel, HardType(UInt(3 bits))))
  }

}
