package Chainsaw.dspFlow

import scala.collection.JavaConverters._

sealed trait DspVertexType
case object NoOpType extends DspVertexType

abstract class DspVertex(implicit dfg: DspFlow) {

  // attributes
  var name: String = _

  def inCount: Int

  def outCount: Int

  def delay: Int
  def executionTime: Double

  // naming & readability
  override def toString: String = name

  // utils

  def incomingEdges: Seq[DspEdge] = dfg.incomingEdgesOf(this).asScala.toSeq
  def outgoingEdges: Seq[DspEdge] = dfg.outgoingEdgesOf(this).asScala.toSeq

  // init behavior
  dfg.addVertex(this)

}

class NoOp(implicit dfg: DspFlow) extends DspVertex {
  this.name = "NoOp"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def inCount: Int = 1

  override def outCount: Int = 1
}

class Inter(signalName: String)(implicit dfg: DspFlow) extends NoOp {
  this.name = signalName
}

class Constant(val value: Double)(implicit dfg: DspFlow) extends DspVertex {
  this.name = value.toString

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

  override def inCount: Int = 0

  override def outCount: Int = 1
}

////////////////////
// Numeric operators
////////////////////

abstract class BinaryOp(implicit dfg: DspFlow) extends DspVertex {

//  override def name_=(newName: String): Unit = throw new Exception("name of BinaryOp should not be changed")

  override def inCount: Int  = 2
  override def outCount: Int = 1
}

class Add(implicit dfg: DspFlow) extends BinaryOp {
  this.name = "Add"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0

}

class Mult(implicit dfg: DspFlow) extends BinaryOp {
  this.name = "Mult"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0
}

class ConstMult(val constant: Double)(implicit dfg: DspFlow) extends BinaryOp {
  this.name = f"X$constant%.2f"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0
}

class Sub(implicit dfg: DspFlow) extends BinaryOp {
  this.name = "Sub"

  override val delay: Int            = 0
  override val executionTime: Double = 0.0
}

////////////////////
// Periodic modules
////////////////////

class PeriodicRam(val depth: Int, val writeAddress: Seq[Int], val readAddress: Seq[Int])(implicit dfg: DspFlow)
    extends DspVertex {
  this.name = f"PRAM"

  override val delay: Int            = 0 // TODO: proper?
  override val executionTime: Double = 0.0

  override def inCount: Int = 1

  override def outCount: Int = 1
}

class PeriodicRom(val depth: Int, val data: Seq[Double])(implicit dfg: DspFlow) extends DspVertex {
  this.name = f"PROM"

  override val delay: Int            = 0 // TODO: proper?
  override val executionTime: Double = 0.0

  override def inCount: Int = 0

  override def outCount: Int = 1
}

class StreamedPermutation(val permutation: Seq[Int], val parallel: Int)(implicit dfg: DspFlow) extends DspVertex {
  this.name = f"SP"

  override val delay: Int            = 0 // TODO: use actual delay
  override val executionTime: Double = 0.0

  override def inCount: Int = parallel

  override def outCount: Int = parallel
}
