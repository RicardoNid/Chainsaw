package Chainsaw.algorithms

import org.jgrapht.alg.color.SmallestDegreeLastColoring
import org.jgrapht.graph._
import spinal.core.{True, isPow2, log2Up}

import scala.jdk.CollectionConverters.mapAsScalaMapConverter

case class Swap2[T](swap: Boolean) extends Transform[T] {
  override val sizeIn: Int  = 2
  override val sizeOut: Int = 2

  override def transform(dataIn: Seq[T]): Seq[T] = if (swap) dataIn.reverse else dataIn

  override def toString: String = if (swap) "<->" else "|-|"
}

object Benes {

  // TODO: simplify
  def getBenesControl[T](permutation: Permutation[T]): Seq[Seq[Boolean]] = {

    val permuted = permutation.permuted
    require(isPow2(permutation.sizeIn))
    val n = permuted.size

    if (n == 2) {                  // base solution
      Seq(Seq(permuted.head == 1)) // for 0,1 -> false, else(1,0) true
    } else {

      // solve current stage problem as by coloring a graph
      val colorGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      permuted.indices.foreach(colorGraph.addVertex) // vertices
      // add constraints by adding edges(connected vertices shouldn't have the same color)
      (0 until n / 2).foreach(i => colorGraph.addEdge(i, i + n / 2))                     // input side constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(permuted(i), permuted(i + n / 2))) // output side constraint

      // TODO: find best algo for coloring in this problem
      val color: Seq[(Int, Integer)] =
        new SmallestDegreeLastColoring(
          colorGraph
        ).getColoring.getColors.asScala.toSeq // solve the problem, get pairs of vertex->color
      require(
        color.forall(_._2 < 2),
        s"there are ${color.map(_._2).max + 1} > 2 colors in solution"
      ) // 2-color requirement

      /** -------- solution extraction
        * --------
        */
      val up                         = color.filter(_._2 == 0).map(_._1) // vertices colored 0
      val bottom                     = color.filter(_._2 == 1).map(_._1) // vertices colored 1
      val solutionPre: Seq[Boolean]  = up.sortBy(_ % (n / 2)).map(_ >= (n / 2))
      val solutionPost: Seq[Boolean] = up.map(permuted.indexOf(_)).sortBy(_ % (n / 2)).map(_ >= (n / 2))

      // sub-problem construction
      val problem0 = up.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val problem1 = bottom.sortBy(permuted.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val solutionMid: Seq[Seq[Boolean]] =
        getBenesControl(Permutation(problem0))         // upper sub-network
          .zip(getBenesControl(Permutation(problem1))) // low sub-network
          .map { case (s0, s1) => s0 ++ s1 }

      solutionPre +: solutionMid :+ solutionPost
    }
  }

  def getBenes[T](permutation: Permutation[T]): TransformList[T] = {
    def impl(controlBits: Seq[Seq[Boolean]]): TransformList[T] = {

      if (controlBits.length == 1) Swap2[T](controlBits.head.head)
      else {
        val shuffle = LinearPermutation.perfectShuffle[T](log2Up(controlBits.head.length * 2))
        val swap0   = controlBits.head.map(Swap2[T]).map(transform2List).reduce(_ ++ _)
        val swap1   = controlBits.last.map(Swap2[T]).map(transform2List).reduce(_ ++ _)
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

  // TODO: a method to quantify the complexity of current network
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
