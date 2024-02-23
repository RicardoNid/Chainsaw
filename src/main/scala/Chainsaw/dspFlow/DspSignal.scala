package Chainsaw.dspFlow

import breeze.math.{Complex => BComplex}

import scala.language.experimental.macros

case class Signal(v: DspVertex, outId: Int) {
  def :=(src: Signal)(implicit dfg: DspFlow): Unit = {
    val e = new DspEdge(0, src.outId, 0)
    dfg.addEdge(src.v, v, e)
  }

  def d(delay: Int = 1)(implicit dfg: DspFlow): Signal = {
    val e    = new DspEdge(delay, outId, 0)
    val noOp = new NoOp()
    dfg.addEdge(v, noOp, e)
    Signal(noOp, 0)
  }

  // basic operators
  def unaryOp(operator: DspVertex)(implicit dfg: DspFlow): Signal = {
    val e = new DspEdge(0, this.outId, 0) // TODO: merge existing delay
    dfg.addEdge(this.v, operator, e)
    Signal(operator, 0)
  }
  def binaryOp(operator: DspVertex, that: Signal)(implicit dfg: DspFlow): Signal = {
    val e0 = new DspEdge(0, this.outId, 0) // TODO: merge existing delay
    val e1 = new DspEdge(0, that.outId, 1)
    dfg.addEdge(this.v, operator, e0)
    dfg.addEdge(that.v, operator, e1)
    Signal(operator, 0)
  }

  // numeric
  def +(that: Signal)(implicit dfg: DspFlow): Signal = binaryOp(new Add, that)
  def -(that: Signal)(implicit dfg: DspFlow): Signal = binaryOp(new Sub, that)

  def *(that: Signal)(implicit dfg: DspFlow): Signal = binaryOp(new Mult, that)

  def *(that: Double)(implicit dfg: DspFlow): Signal = unaryOp(new ConstMult(that))

// TODO: utils accessing signal information & SpinalHDL Data
  // selection
}

object S {
  def apply(name: String)(implicit dfg: DspFlow): Signal = Signal(new Inter(name), 0)
  def apply()(implicit dfg: DspFlow): Signal             = Signal(new Inter("anon"), 0)

}



case class ComplexSignal(real: Signal, imag: Signal)(implicit dfg: DspFlow) {

  def :=(src: ComplexSignal): Unit = {
    real := src.real
    imag := src.imag
  }

  def d(delay: Int = 1): ComplexSignal      = ComplexSignal(real.d(delay), imag.d(delay))
  def +(that: ComplexSignal): ComplexSignal = ComplexSignal(real + that.real, imag + that.imag)
  def -(that: ComplexSignal): ComplexSignal = ComplexSignal(real - that.real, imag - that.imag)
  def *(that: ComplexSignal): ComplexSignal =
    ComplexSignal(real * that.real - imag * that.imag, real * that.imag + imag * that.real)
  def *(that: Signal): ComplexSignal = ComplexSignal(real * that, imag * that)
  def *(that: Double): ComplexSignal = ComplexSignal(real * that, imag * that)
  def *(that: BComplex): ComplexSignal =
    ComplexSignal(real * that.real - imag * that.imag, real * that.imag + imag * that.real)
}

object CS {
  def apply(name: String)(implicit dfg: DspFlow): ComplexSignal =
    ComplexSignal(S(name + "_real"), S(name + "_imag"))

}
