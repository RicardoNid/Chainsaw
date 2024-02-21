package Chainsaw.dspFlow

import spinal.core._
import spinal.lib.experimental.math._
import spinal.lib.{Stream, StreamJoin, _}

import scala.collection.mutable

object DspElaboration {

  def apply(dfg: DspFlow) = {

    import dfg._

    def implVertex[T <: Data](v: DspVertex, signalMap: mutable.Map[DspVertex, Seq[Stream[T]]]): Unit = {
      if (v.incomingEdges.nonEmpty) {
        // numeric function implementation
        // step1: get inputs
        val inputs: Seq[Stream[T]] = v.incomingEdges
          .sortBy(_.inId)
          .map(e => (signalMap(e.source)(e.outId), e.delay))
          .map { case (stream, d) => Seq.iterate(stream, d + 1)(_.m2sPipe()).last }
        // step2: datapath TODO: pay attention when dfgVertex has inner delay
        val rets: Seq[T] = VertexImplementation.impl(v, inputs.map(_.payload))

        // handshake implementation
        // step1: sync by StreamJoin
        inputs.foreach(_.ready.allowOverride) // inputs may contain repeated signals
        val retStream = Seq.iterate(StreamJoin(inputs), v.delay + 1)(_.m2sPipe()).last
        signalMap(v).zip(rets).foreach { case (placeholder, ret) =>
          placeholder.payload := ret
          placeholder.valid   := retStream.valid
          retStream.ready     := placeholder.ready
        }
        // TODO: step2: sync by StreamFork}
        if (useStream) {}
      }

    }

    // 1. numeric type inference
    if (!useFloating) {
      // TODO: implement fixed point numeric type inference
    }

    // 2. signal initialization
    if (useFloating) {
      vertexSeq.foreach {
        case const: Constant =>
          val stream = Stream(Floating32())
          stream.valid   := True
          stream.payload := const.value
          floatingMap += ((const, Seq(stream)))
        case v: DspVertex => floatingMap += ((v, Seq.fill(v.outCount)(Stream(Floating32()))))
      }
    } else {

      // TODO: implement fixed point signal initialization
    }
    // 3. implement vertices one by one(except constant vertex)
    if (useFloating) vertexSeq.filterNot(_.isInstanceOf[Constant]).foreach(v => implVertex(v, floatingMap))
    else {
      // TODO: implement fixed point vertex implementation
    }

    // 4. set input & output
    if (isTop) {
      val inputs  = vertexSeq.filter(v => v.incomingEdges.isEmpty && !v.isInstanceOf[Constant])
      val outputs = vertexSeq.filter(v => v.outgoingEdges.isEmpty)
      inputs.foreach(i  => println(s"input: ${i.name}"))
      outputs.foreach(o => println(s"output: ${o.name}"))
      if (useFloating) {
        inputs.foreach { i =>
          slave(floatingMap(i)(0))
          floatingMap(i)(0).setName(i.name)
        }
        outputs.foreach { o =>
          master(floatingMap(o)(0))
          floatingMap(o)(0).setName(o.name)
        }
      } else {
        // TODO: implement fixed point input & output
      }
    }

    // 5. set RTL names
  }

}
