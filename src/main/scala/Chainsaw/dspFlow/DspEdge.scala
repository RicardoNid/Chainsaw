package Chainsaw.dspFlow

class DspEdge(
    // attributes
    val delay: Int,
    val outId: Int,
    val inId: Int
)(implicit dfg: DspFlow) {

  // naming & readability
  override def toString: String = delay.toString // TODO: show dataType by name

  // utils
  def source: DspVertex = dfg.getEdgeSource(this)
  def target: DspVertex = dfg.getEdgeTarget(this)

}
