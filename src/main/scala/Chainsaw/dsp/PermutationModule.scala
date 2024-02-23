package Chainsaw.dsp

import Chainsaw.DataUtil
import Chainsaw.algorithms._
import Chainsaw.memory.StreamedPIPO
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._

import scala.collection.immutable
import scala.language.postfixOps
import scala.reflect.ClassTag

// TODO: implement this module
//  1. for different configurations
//  2. making this module stream-based and halt-able
//  3. making this module an operator for DSP-DSL
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
            BuildLinearPermutation(dataIn, dataOut, lp)
            latency = period
          } else ??? // using registers & switches
        case None => ??? // Arbitrary Permutation
      }
    }
  }

  require(latency >= 0)

}

// TODO: ping-pong and back-pressure
// TODO: make combinational logic as functions, build the datapath by Stream operations(mainly, translateWith)
object BuildLinearPermutation {
  def apply[T <: Data: ClassTag](
      streamIn: Stream[Vec[T]],
      streamOut: Stream[Vec[T]],
      lp: LinearPermutation[T]
  ): Unit = {

    // building block for connection networks
    def switch2(a: Bits, b: Bits, swap: Bool): (Bits, Bits) = (Mux(swap, b, a), Mux(swap, a, b))

    val parallel = streamIn.payload.size
    val period   = lp.sizeIn / parallel

    val dataType = HardType(Bits(streamIn.payload.head.getBitsWidth bits))

    val s                                               = log2Up(parallel)
    val (n, k, connM, connN, writeAddress, readAddress) = lp.solve(s)

    // components
    val counterWrite = Counter(period, inc = streamIn.fire)
    val counterRead  = Counter(period, inc = streamIn.fire) // FIXME: should be decided by the fire signal after RAN

    def throughNetwork( //  TODO: pipelined version
        description: Array[(immutable.IndexedSeq[Array[Int]], String)],
        symbol: Char,
        data: Vec[Bits]
    ) = {

      def str2Bit(str: String): Bool = {
        val index = str.split("_").last.toInt
        require(index >= s)
        if (symbol == 'W') counterWrite.value(index - s)
        else counterRead.value(index - s)
      }

      var current = Array(data: _*)
      val next    = Array(data: _*)
      description.zipWithIndex.foreach { case ((switches, control), i) =>
        val controlBit = str2Bit(control) // parsing control bit
        controlBit.setName(s"control_$symbol$i")
        controlBit.simPublic()
        switches.foreach { switch =>
          val Array(i, j, x, y) = switch
          val (a, b)            = switch2(current(i), current(j), controlBit)
          next.update(x, a)
          next.update(y, b)
        }
        current = next
      }
      Vec(next)
    }

    def goThroughRAM(data: Stream[Vec[Bits]]) = {
      data.ready.allowOverride()
      val subStreams = (0 until data.payload.length).map { i =>
        val subStream: Stream[Bits] = data.map(_.apply(i))
        val bits                    = subStream.payload
        val (index, payload)        = bits.splitAt(s)
        val pipo                    = new StreamedPIPO(dataType, period)
        pipo.readAddr := pipo.readIndex
        val v                   = pipo.writeIndex @@ index.asUInt
        val indices: Array[Int] = writeAddress.split(",").map(_.split("_").last.toInt)
        val writeAddr           = indices.map(v(_)).map(_.asBits).reduce(_ ## _).asUInt
        pipo.writeAddr := writeAddr
        pipo           << subStream.map(_.takeHigh(subStream.payload.getBitsWidth - s))
        pipo.streamOut
      }
      StreamJoin.vec(subStreams) // TODO: reductive AND off subStreams redundant
    }

    // streamed data path
    val streamWithIndex = streamIn.map(vec =>
      Vec(vec.zipWithIndex.map { case (data, index) => data.asBits ## B(index, log2Up(parallel) bits) })
    )
    val streamAfterConnM = streamWithIndex.map(throughNetwork(connM, 'W', _))
    val streamAfterRam   = goThroughRAM(streamAfterConnM)
    val streamAfterConnN = streamAfterRam.map(throughNetwork(connN, 'R', _))
    streamOut.arbitrationFrom(streamAfterConnN)
    streamOut.payload.zip(streamAfterConnN.payload).foreach { case (out, data) => out.assignFromBits(data) }

  }
}
