package Chainsaw.dspFlow

import spinal.core.Data
import spinal.lib.experimental.math.Floating

object VertexImplementation {

  def implFloating(vertex: DspVertex, dataIn: Seq[Floating]): Seq[Floating] = {

    import Chainsaw.arithmetic.floating.FloatingOpsChisel._

    val name = vertex.name.toLowerCase
    name match {
      case "add" => Seq(dataIn(0) + dataIn(1))
      case "sub" => Seq(dataIn(0) - dataIn(1))
      case "mul" => Seq(dataIn(0) * dataIn(1))
      case _ =>
        if (name.startsWith("X")) Seq(dataIn(0) * vertex.asInstanceOf[ConstMult].constant.toFloat)
        else dataIn // for NoOp
    }

  }

  def impl[T <: Data](vertex: DspVertex, dataIn: Seq[T]): Seq[T] = dataIn.head match {
    case floating: Floating => implFloating(vertex, dataIn.asInstanceOf[Seq[Floating]]).asInstanceOf[Seq[T]]
  }

}
