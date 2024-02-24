package Chainsaw.dsp

import Chainsaw.DataUtil
import Chainsaw.algorithms._
import Chainsaw.memory.StreamedPIPOFsm
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._

import scala.collection.immutable
import scala.language.postfixOps
import scala.reflect.ClassTag

// TODO: implement this module
//  1. for different configurations
//  2. making this module stream-based and halt-able - need a Stream test suite
//  3. making this module an operator for DSP-DSL and a sub component of FindPeak
case class PermutationModule[T <: Data: ClassTag](
    permuted: Seq[Int],
    parallel: Int,
    dataType: HardType[T]
) extends Module {

  val dataIn  = slave(Stream(Vec(dataType, parallel)))
  val dataOut = master(Stream(Vec(dataType, parallel)))

  val permutation = Permutation[T](permuted)
  val period      = permuted.size / parallel
  val isBase2     = isPow2(parallel) && isPow2(permuted.size)
  var latency     = -1

  if (parallel == permuted.size) { // hard-wired
    val permuted = Vec(permutation.transform(dataIn.payload))
    dataIn.translateWith(permuted) >> dataOut
    latency = 0
  } else if (parallel == 1) { // serial permutation using RAM
    ???
  } else {
    if (isBase2) {
      LinearPermutation.fromPermutation(permutation) match { // Linear Permutation
        case Some(lp) =>
          val usingRAMs = true // TODO: judge
          if (usingRAMs) { // using RAMs
            latency = BuildLinearPermutation(dataIn, dataOut, lp)
          } else ??? // using registers & switches
        case None => ??? // Arbitrary Permutation
      }
    }
  }
  require(latency >= 0)
}

object BuildLinearPermutation {
  def apply[T <: Data: ClassTag](
      streamIn: Stream[Vec[T]],
      streamOut: Stream[Vec[T]],
      lp: LinearPermutation[T]
  ): Int = {

    // building block for connection networks
    def switch2(a: Bits, b: Bits, swap: Bool): (Bits, Bits) = (Mux(swap, b, a), Mux(swap, a, b))

    // parameters and the solution
    val parallel                                        = streamIn.payload.size
    val period                                          = lp.sizeIn / parallel
    val s                                               = log2Up(parallel)
    val (n, k, connM, connN, writeAddress, readAddress) = lp.solve(s) // solution describing how this module works

    // building methods

    // zip data with index(where in stage) for writeAddr generation
    def withIndex(data: Vec[T]) = Vec(data.zipWithIndex.map { case (data, index) => data.asBits ## B(index, s bits) })

    def goThroughNetwork(
        description: Array[(immutable.IndexedSeq[Array[Int]], String)],
        stageCount: UInt,
        data: Stream[Vec[Bits]]
    ) = {

      // bundle data and the counter value, so that they can be pipelined together
      case class DataWithCount(dataWidth: Int, parallel: Int, countWidth: Int) extends Bundle {
        val data  = Vec(Bits(dataWidth bits), parallel)
        val count = UInt(countWidth bits)
      }
      val bitWidth = data.payload.head.getBitsWidth
      def getBundle(data: Vec[Bits], count: UInt) = {
        val ret = DataWithCount(bitWidth, parallel, stageCount.getWidth)
        ret.data  := data
        ret.count := count
        ret
      }
      // TODO: to avoid the boilerplate below, we may need an "easy bundle" type <: Data which is a tuple of multiple data

      val streamIn = data.translateWith(getBundle(data.payload, stageCount))
      // stage by stage spatial permutation
      val ret = description.foldLeft(streamIn) { case (stream, (switches, control)) =>
        // get control bit from its description
        val payload    = stream.payload.data
        val count      = stream.payload.count
        val index      = control.split("_").last.toInt
        val controlBit = count(index - s)
        val next       = Array(payload: _*) // using Array as it can be updated
        switches.foreach { switch =>
          val Array(i, j, x, y) = switch
          val (a, b)            = switch2(payload(i), payload(j), controlBit)
          next.update(x, a)
          next.update(y, b)
        }
        stream.translateWith(getBundle(Vec(next), count)).m2sPipe()
      }
      ret.map(_.data)
    }

    def goThroughRAM(data: Stream[Vec[Bits]], pipoFsm: StreamedPIPOFsm) = {
      // as different RAM need different write address, cannot use a StreamedFIFO[Vec[Bits]], instead, we instantiated RAMs separately, but shared the same PIPO FSM
      val ret: Seq[Bits] = (0 until data.payload.length).map { i =>
        val payload             = data.payload(i)
        val (bits, index)       = payload.splitAt(s)
        val ping, pong          = Mem(HardType(bits), period)
        val readAddr            = pipoFsm.readIndex
        val inputVector         = pipoFsm.writeIndex @@ index.asUInt // the [x_2|x_1] vector in paper
        val indices: Array[Int] = writeAddress.split(",").map(_.split("_").last.toInt)
        val writeAddr           = indices.map(inputVector(_)).map(_.asBits).reduce(_ ## _).asUInt
        // RAM operations
        ping.write(writeAddr, bits, streamIn.fire && pipoFsm.writePing)
        pong.write(writeAddr, bits, streamIn.fire && !pipoFsm.writePing)
        Mux(pipoFsm.readPing, ping.readAsync(readAddr), pong.readAsync(readAddr))
      }
      pipoFsm.streamOut.translateWith(Vec(ret))
    }

    // streamed data path
    val streamWithIndex  = streamIn.map(withIndex)
    val counterWrite     = Counter(period, inc = streamWithIndex.fire)
    val streamAfterConnM = goThroughNetwork(connM, counterWrite.value, streamWithIndex)
    val pipoFsm          = StreamedPIPOFsm(period)
    pipoFsm << streamAfterConnM
    val streamAfterRam   = goThroughRAM(streamAfterConnM, pipoFsm)
    val counterRead      = Counter(period, inc = streamAfterRam.fire)
    val streamAfterConnN = goThroughNetwork(connN, counterRead.value, streamAfterRam)
    streamOut.arbitrationFrom(streamAfterConnN)
    streamOut.payload.zip(streamAfterConnN.payload).foreach { case (out, data) => out.assignFromBits(data) }

    val latency = connM.length + // for connection network M
      period +     // for PIPO
      connN.length // for connection network N
    latency
  }
}
