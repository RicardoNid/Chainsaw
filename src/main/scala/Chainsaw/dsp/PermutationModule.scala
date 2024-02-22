package Chainsaw.dsp

import Chainsaw.algorithms._
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._

import scala.collection.immutable
import scala.language.postfixOps
import scala.reflect.ClassTag

object BuildLinearPermutation {
  def apply[T <: Data: ClassTag](streamIn: Stream[Vec[T]], streamOut: Stream[Vec[T]], lp: LinearPermutation[T]) = {

    val dataIn   = streamIn.payload
    val dataOut  = streamOut.payload
    val parallel = dataIn.size
    val period   = lp.sizeIn / parallel
//    def switch2(a: Bits, b: Bits, swap: Bool): (Bits, Bits) = (Mux(swap, b, a), Mux(swap, a, b))
    def switch2(a: Bits, b: Bits, swap: Bool): (Bits, Bits) = (Mux(swap, a, b), Mux(swap, b, a))
    val dataType                                            = HardType(Bits(dataIn.head.getBitsWidth bits))

    val s                                               = log2Up(parallel)
    val (n, k, connM, connN, writeAddress, readAddress) = lp.solve(s)
    val counter = Counter(period, inc = streamIn.valid) // TODO: two counter for read and write
    counter.value.setName("counter")
    counter.value.simPublic()
    val dataWithIndex: IndexedSeq[Bits] = dataIn.zipWithIndex.map { case (data, index) =>
      data.asBits ## B(index, log2Up(parallel) bits)
    }

    var current = Array(dataWithIndex: _*)
    val next    = Array(dataWithIndex: _*)

    def str2Bit(str: String): Bool = {
      val index = str.split("_").last.toInt
      require(index >= s)
      counter.value(index - s)
    }

    def buildNetwork(description: Array[(immutable.IndexedSeq[Array[Int]], String)], symbol: Char) = {
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
    }

    buildNetwork(connM, 'W') // network for writing
    // RAMs TODO: ping-pong
    val rams = (0 until parallel).map(i => Mem(dataType, period))
    // writing
    rams.zip(current).zipWithIndex.foreach { case ((ram, bits), i) =>
      val v                   = counter.value @@ bits.takeLow(s).asUInt
      val data                = bits.takeHigh(bits.getWidth - s)
      val indices: Array[Int] = writeAddress.split(",").map(_.split("_").last.toInt)
      val addr                = indices.map(v(_)).map(_.asBits).reduce(_ ## _).asUInt
      addr.setName(s"writingAddress$i")
      addr.simPublic()
      ram.write(addr, data)
    }
    // reading
    rams.zipWithIndex.foreach { case (ram, i) =>
      val addr = counter.value // when N4 is identity matrix and N3 is all-zero, the reading address is counter.value
      next.update(i, ram.readAsync(addr))
    }
    current = next
    // network for reading
    buildNetwork(connN, 'R')
    // output
    dataOut.zip(current).foreach { case (out, data) => out.assignFromBits(data) }
    streamOut.valid.set()
    streamIn.ready.set()
  }
}

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
