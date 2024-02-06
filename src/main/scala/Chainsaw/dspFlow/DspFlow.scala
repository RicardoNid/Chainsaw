package Chainsaw.dspFlow

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math.Floating

import scala.collection.JavaConverters._
import scala.collection.mutable

class DspFlow extends DirectedWeightedPseudograph[DspVertex, DspEdge](classOf[DspEdge]) with Area {

  implicit val background: DspFlow = this

  // attributes
  var useFloating = false
  var useStream   = false

  def vertexSeq: Seq[DspVertex] = vertexSet().asScala.toList
  def edgeSeq: Seq[DspEdge]     = edgeSet().asScala.toList

  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  def exportDrawIo(name: String): Unit = DspFlowToDrawIo(this, name)

  def clarify() = {
    // rule0: remove redundant SISO NoOps
  }

  def build() = {}
}
