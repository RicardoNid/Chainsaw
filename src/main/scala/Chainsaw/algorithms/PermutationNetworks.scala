package Chainsaw.algorithms

import org.jgrapht.alg.color.SmallestDegreeLastColoring
import org.jgrapht.graph._
import spinal.core.{isPow2, log2Up}

import scala.jdk.CollectionConverters.mapAsScalaMapConverter

case class Butterfly[T](swap: Boolean) extends Transform[T] {
  override val sizeIn: Int  = 2
  override val sizeOut: Int = 2

  override def transform(dataIn: Seq[T]): Seq[T] = if (swap) dataIn.reverse else dataIn

  override def symbol: String = if (swap) "<->" else "|-|"
}

object Benes {

  /** Get the stage-by-stage control bits for a Benes network, according to a given permutation
    *
    * the solution is not unique
    * @return
    *   a 2-dimensional sequence of control bit, each row represents a stage, each entry represents a control bit, True
    *   for swap and False for no swap
    */
  def getBenesControl[T](permutation: Permutation[T]): Seq[Seq[Boolean]] = {

    val permuted = permutation.permuted
    require(isPow2(permutation.sizeIn))
    val n = permuted.size

    if (n == 2) {                  // base solution
      Seq(Seq(permuted.head == 1)) // for 0,1 -> false, else(1,0) true
    } else {

      ////////////////////
      // get a partition of packets for upper/lower sub-network by coloring a graph
      ////////////////////
      val colorGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      permuted.indices.foreach(colorGraph.addVertex) // vertices
      // add constraints by adding edges(connected vertices shouldn't have the same color)
      (0 until n / 2).foreach(i => colorGraph.addEdge(i, i + n / 2))                     // input constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(permuted(i), permuted(i + n / 2))) // output constraint
      // coloring,get pairs of vertex->color
      val color: Seq[(Int, Integer)] = new SmallestDegreeLastColoring(colorGraph).getColoring.getColors.asScala.toSeq
      // 2-color requirement, in case the JGraphtT algorithm fails
      require(color.forall(_._2 < 2), s"there are ${color.map(_._2).max + 1} > 2 colors in solution")
      val Seq(up, bottom) = Seq(0, 1).map(c => color.filter(_._2 == c).map(_._1))

      ////////////////////
      // using the partition
      ////////////////////
      // control bits of first/last stage
      val permutedUp                     = up.map(permuted.indexOf(_))
      val Seq(solutionPre, solutionPost) = Seq(up, permutedUp).map { ps => ps.sortBy(_ % (n / 2)).map(_ >= (n / 2)) }
      // sub-problems
      val Seq(problemUp, problemBottom) = Seq(up, bottom).map(_.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2)))
      val Seq(solutionUp, solutionBottom) = Seq(problemUp, problemBottom).map(p => getBenesControl(Permutation(p)))
      val solutionMid                     = solutionUp.zip(solutionBottom).map { case (up, bottom) => up ++ bottom }
      // merge solutions
      solutionPre +: solutionMid :+ solutionPost
    }
  }

  def getBenes[T](permutation: Permutation[T]): TransformList[T] = {
    def impl(controlBits: Seq[Seq[Boolean]]): TransformList[T] = {

      if (controlBits.length == 1) Butterfly[T](controlBits.head.head)
      else {
        val shuffle = LinearPermutation.perfectShuffle[T](log2Up(controlBits.head.length * 2))
        val swap0   = controlBits.head.map(Butterfly[T]).map(transform2List).reduce(_ ++ _)
        val swap1   = controlBits.last.map(Butterfly[T]).map(transform2List).reduce(_ ++ _)
        val pre     = shuffle * swap0 * shuffle.getInverse
        val post    = shuffle * swap1 * shuffle.getInverse
        val half    = controlBits.head.length / 2
        val mid0    = impl(controlBits.drop(1).dropRight(1).map(_.take(half)))
        val mid1    = impl(controlBits.drop(1).dropRight(1).map(_.takeRight(half)))
        pre * (mid0 ++ mid1) * post
      }

    }
    impl(getBenesControl(permutation))
  }
}

object PermutationNetworks {

  /** this method find the most efficient network for a series of give permutations
    */
  def getNetwork[T](permutations: Seq[Permutation[T]], parallel: Int): TransformList[T] = {
    require(permutations.forall(_.sizeIn == permutations.head.sizeIn))
    if (permutations.length == 1 && permutations.head.sizeIn == parallel) permutations.head // hard-wired
    else ???                                                                                // TODO: more networks
  }

}
