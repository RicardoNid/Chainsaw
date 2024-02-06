package Chainsaw.dspFlow

import Chainsaw.NumericType
import spinal.lib.experimental.math.Floating

import scala.collection.JavaConverters._

abstract class DspVertex(implicit dfg: DspFlow) {

  // attributes
  val name: String

  def inCount: Int

  def outCount: Int

  def delay: Int
  def executionTime: Double

  // naming & readability
  override def toString: String = name // TODO: get name by reflection

  // utils

  def incomingEdges: Seq[DspEdge] = dfg.incomingEdgesOf(this).asScala.toSeq
  def outgoingEdges: Seq[DspEdge] = dfg.outgoingEdgesOf(this).asScala.toSeq

  // init behavior
  dfg.addVertex(this)

}

class NoOp(implicit dfg: DspFlow) extends DspVertex {
  override val name: String = "NoOp"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def inCount: Int = 1

  override def outCount: Int = 1
}

class Add(implicit dfg: DspFlow) extends DspVertex {
  override val name: String = "Add"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def inCount: Int = 2

  override def outCount: Int = 1
}

class Mult(implicit dfg: DspFlow) extends DspVertex {
  override val name: String = "Mult"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def inCount: Int = 2

  override def outCount: Int = 1
}
