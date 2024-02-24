package Chainsaw.memory

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.fsm._
import spinal.lib.bus._

import scala.language.postfixOps

// TODO: verification
// TODO: documentation
case class StreamedPIPOFsm(depth: Int) extends Module {

  // the payload won't be actually used, actually we can use a valid and a ready instead, but for the consistency, we use a Stream
  val streamIn  = slave(Stream(Bool()))
  val streamOut = master(Stream(Bool()))
  streamIn.payload.allowOverride()
  streamOut.payload.allowOverride()

  // state indicator
  val writeCounter        = Counter(depth, inc = streamIn.fire)
  val readCounter         = Counter(depth, inc = streamOut.fire)
  val writeIndex          = out(writeCounter.value)
  val readIndex           = out(readCounter.value)
  val readPing, writePing = out(RegInit(False)) // pointers for ping-pong
  readPing.toggleWhen(readCounter.willOverflow)
  writePing.toggleWhen(writeCounter.willOverflow)

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

  streamOut.payload.set() // need no actual implementation

  def freeRun(): Unit = {
    streamIn.valid.set()
    streamOut.ready.set()
  }

  def <<[T <: Data](stream: Stream[T]): Unit = {
    streamIn.arbitrationFrom(stream)
    streamIn.payload.set() // need no driver
  }
  def >>[T <: Data](stream: Stream[T]): Unit = {
    stream.arbitrationFrom(streamOut)
  }
}

case class StreamedPIPO[T <: Data](dataType: HardType[T], depth: Int) extends Module {

  val streamIn            = slave(Stream(dataType))
  val streamOut           = master(Stream(dataType))
  val writeAddr, readAddr = in UInt (log2Up(depth) bits)

  val fsm = StreamedPIPOFsm(depth)
  fsm << streamIn
  fsm >> streamOut

  val ping, pong = Mem(dataType, depth)

  // RAM operation
  ping.write(writeAddr, streamIn.payload, streamIn.fire && fsm.writePing)
  pong.write(writeAddr, streamIn.payload, streamIn.fire && !fsm.writePing)
  streamOut.payload := Mux(fsm.readPing, ping.readAsync(readAddr), pong.readAsync(readAddr))

  // state indicator
  val writeIndex = out(UInt(log2Up(depth) bits))
  writeIndex := fsm.writeIndex
  val readIndex = out(UInt(log2Up(depth) bits))
  readIndex := fsm.readIndex

  def freeRun(): Unit = {
    streamIn.valid.set()
    streamOut.ready.set()
  }

  def <<(stream: Stream[T]): Unit = stream    >> streamIn
  def >>(stream: Stream[T]): Unit = streamOut >> stream

}
