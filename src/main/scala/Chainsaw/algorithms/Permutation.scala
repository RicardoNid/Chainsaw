package Chainsaw.algorithms

import Chainsaw._
import spinal.core.log2Up

/** generic model for permutation
  * @param permuted
  *   a sequence of integers in [0, N)
  * @example
  *   a permutation defined by [0,2,1,3] permutes [a,b,c,d] to [a,c,b,d]
  */
class Permutation[T](val permuted: Seq[Int]) extends Transform[T] {

  override val sizeIn: Int  = permuted.size
  override val sizeOut: Int = permuted.size

  override def transform(dataIn: Seq[T]): Seq[T] = permuted.map(dataIn.apply)

  def getPermutationMatrix: Matrix[Double] = {
    val content: Array[Array[Double]] = Array.tabulate(sizeOut, sizeOut)((i, j) => if (permuted(i) == j) 1 else 0)
    Matrix(content)
  }

  /** the permutation is linear if a bit matrix exists
    */
  def isLinear: Boolean = getBitMatrix match {
    case Some(value) => true
    case None        => false
  }

  def getBitMatrix: Option[Matrix[GF2]] = {
    // TODO: solve a linear system based on GF2, when size = 2^n, there are n^2 variables and 2^n equations
    None
  }

  /** @see
    *   ''Automatic Generation of Streaming Datapaths for Arbitrary Fixed Permutations, Peter A. Milder, James C. Hoe,
    *   and Markus P¨uschel'', "mapping \Pi_w"
    */
  def getMappingMatrix(streamWidth: Int): Matrix[Double] = {
    val mappintMatrix =
      Array.tabulate(streamWidth, streamWidth) { (i, j) =>
        permuted.zipWithIndex.count { case (out, in) =>
          (out % streamWidth == j) && (in % streamWidth == i)
        }.toDouble
      }
    Matrix(mappintMatrix)
  }
  override def toString: String = "permutation: \n" + (0 until sizeIn).map(i => s"$i -> ${permuted(i)}").mkString("\n")

}

object Permutation {

  def apply[T](permuted: Seq[Int]): Permutation[T] = new Permutation(permuted)
  def identity[T](size: Int): Permutation[T]       = Permutation(0 until size)

  /** generate a random permutation of specific size
    */
  def random[T](size: Int): Permutation[T] = Permutation(scala.util.Random.shuffle((0 until size).toList))
  def matrixInterleave[T](row: Int, col: Int): Permutation[T] = Permutation(
    Array.tabulate(col, row)((i, j) => j * col + i).flatten
  )
}
