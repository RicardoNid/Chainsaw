package Chainsaw.edaFlow.boards.alinx

import Chainsaw.edaFlow.Device._
import Chainsaw.edaFlow._
import Chainsaw.edaFlow.vivado.VivadoTask
import spinal.core._
import spinal.lib.{Counter, CounterFreeRun}

import java.io.File
import scala.language.postfixOps

/** ALINX AXKU041 development board
  * @see
  *   [[https://alinx.com/detail/275]] for sales information and manual
  */
class XC7Z020 extends Component with Board {

  // pins with fixed direction
  lazy val clk                = in Bool ()
  lazy val ps_key_n, pl_key_n = in Bool ()

  // pins without fixed direction
  lazy val alinx40Pin1 = Alinx40Pin() // J15
  lazy val alinx40Pin2 = Alinx40Pin() // J16

  // board definition
  override val xdcFile: File = new File(xdcFileDir, "XC7Z020.xdc")
  override val device: XilinxDevice =
    new XilinxDevice(Series7, "XC7Z020CLG484-2".toLowerCase(), 50 MHz, None)

  override lazy val defaultClockDomain = {
    val clockDomainConfig: ClockDomainConfig =
      ClockDomainConfig(clockEdge = RISING, resetKind = BOOT)
    new ClockDomain(clock = clk, config = clockDomainConfig, frequency = FixedFrequency(50 MHz))
  }

}

case class DataProvider(latency: Int) extends XC7Z020 {

  setDefinitionName(s"DataProvider$latency")

  out(alinx40Pin1)

  defaultClockDomain on {
    val dac = AN9767()
    dac.channel1Wrt := clk
    dac.channel1Clk := clk
    dac.channel2Wrt := clk
    dac.channel2Clk := clk
    alinx40Pin1     := dac.alinx40PinOut

    val running = RegInit(False)
    val start   = pl_key_n.fall()
    when(start)(running.set())

    val counterUs = CounterFreeRun(50)

    val counter = Counter(2000, inc = counterUs.willOverflow & running)
    when(start)(counter.clear())
    when(counter.willOverflow)(running.clear())

    assert(latency >= 910)

    val pulse     = Seq.fill(1)(1) ++ Seq.fill(9)(0)
    val CameraRom = Mem((pulse ++ Seq.fill(latency)(0) ++ Seq.fill(10)(pulse).flatten).padTo(2000, 0).map(B(_, 1 bits)))
    val SampleRom = Mem((Seq.fill(10)(0) ++ Seq.fill(900)(1) ++ Seq.fill(1090)(0)).map(B(_, 1 bits)))
    dac.channel1 := Mux(
      CameraRom.readAsync(counter.value).asBool & running,
      AN9767.getVoltageValue(3.3),
      AN9767.getVoltageValue(0.0)
    )
    dac.channel2 := Mux(
      SampleRom.readAsync(counter.value).asBool & running,
      AN9767.getVoltageValue(2.0),
      AN9767.getVoltageValue(0.0)
    )

  }

}

object DataProvider extends App {
  // generate bitstream for data provider

  VivadoTask.genBitStreamForBoard("DataProvider920", DataProvider(920))
  VivadoTask.genBitStreamForBoard("DataProvider930", DataProvider(930))
  VivadoTask.genBitStreamForBoard("DataProvider945", DataProvider(945))
}
