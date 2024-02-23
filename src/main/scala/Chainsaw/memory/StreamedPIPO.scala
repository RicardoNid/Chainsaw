package Chainsaw.memory

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.language.postfixOps

// TODO: verification
// TODO: a fsm-only version without initializing the RAM
case class StreamedPIPO[T <: Data](dataType: HardType[T], depth: Int) extends Module {

  val streamIn            = slave(Stream(dataType))
  val streamOut           = master(Stream(dataType))
  val writeAddr, readAddr = in UInt (log2Up(depth) bits)

  val writeCounter        = Counter(depth, inc = streamIn.fire)
  val readCounter         = Counter(depth, inc = streamOut.fire)
  val readPing, writePing = RegInit(False) // pointers for ping-pong
  readPing.toggleWhen(readCounter.willOverflow)
  writePing.toggleWhen(writeCounter.willOverflow)
  val ping, pong = Mem(dataType, depth)

  val fsm = new StateMachine {
    val empty      = new State with EntryPoint
    val half, full = new State

    empty.whenIsActive(when(writeCounter.willOverflow)(goto(half)))
    half.whenIsActive(
      when(writeCounter.willOverflow && readCounter.willOverflow)(goto(half))
        .elsewhen(writeCounter.willOverflow)(goto(full))
        .elsewhen(readCounter.willOverflow)(goto(empty))
    )
    full.whenIsActive(when(readCounter.willOverflow)(goto(half)))

    streamIn.ready  := isActive(empty) || isActive(half)
    streamOut.valid := isActive(full) || isActive(half)
  }

  // RAM operation
  ping.write(writeAddr, streamIn.payload, streamIn.fire && writePing)
  pong.write(writeAddr, streamIn.payload, streamIn.fire && !writePing)
  streamOut.payload := Mux(readPing, ping.readAsync(readAddr), pong.readAsync(readAddr))

  // state indicator
  val writeIndex = out(writeCounter.value)
  val readIndex  = out(readCounter.value)

  def freeRun(): Unit = {
    streamIn.valid.set()
    streamOut.ready.set()
  }

  def <<(stream: Stream[T]): Unit = stream    >> streamIn
  def >>(stream: Stream[T]): Unit = streamOut >> stream

}

object StreamedPIPO {

  case class StreamedPIPOExample() extends Module {
    val io = new Bundle {
      val in  = slave(Stream(UInt(8 bits)))
      val out = master(Stream(UInt(8 bits)))
    }
    val pipo = StreamedPIPO(UInt(8 bits), 16)
    pipo.writeAddr := pipo.writeIndex
    pipo.readAddr  := pipo.readIndex
    pipo           << io.in
    pipo           >> io.out
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(StreamedPIPOExample())
  }

}
