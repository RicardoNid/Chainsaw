package Chainsaw.permutation

import breeze.linalg._

/** generic model for permutation
  * @param permuted
  *   a sequence of integers in [0, N)
  * @example
  *   a permutation defined by [0,2,1,3] permutes [a,b,c,d] to [a,c,b,d]
  */
class Permutation(val permuted: Seq[Int]) {

  val size = permuted.size
  require(permuted.sorted.equals(permuted.indices))

  /** do permutation
    * @return
    *   permuted data
    */
  def permute[T](dataIn: Seq[T]): Seq[T] = permuted.map(dataIn.apply)

  /** serial connection of permutations
    */
  def cascade(that: Permutation) = {
    require(this.size == that.size, s"size mismatch: ${this.size} != ${that.size}")
    Permutation(that.permute(permuted))
  }

  def *(that: Permutation): Permutation = this.cascade(that)

  def ^(times: Int): Permutation = Seq.fill(times)(this).reduce(_.cascade(_)) // power

  /** parallel connection of permutations
    * @note
    *   this is not the same as concatenation
    * @example
    *   [0,1,2,3] ++ [3,2,1,0] = [0,1,2,3,7,6,5,4]
    */
  def ++(that: Permutation): Permutation = Permutation(this.permuted ++ that.permuted.map(_ + this.size))

  def kronecker(times: Int): Permutation = Seq.fill(times)(this).reduce(_ ++ _)
  def ⊗(times: Int): Permutation         = kronecker(times) // kronecker product

  def getPermutationMatrix = {
    val content = Array.tabulate(size, size)((i, j) => if (permuted(i) == j) 1 else 0)
    new DenseMatrix(size, size, content.flatten)
  }

  /** @see
    *   ''Automatic Generation of Streaming Datapaths for Arbitrary Fixed Permutations, Peter A. Milder, James C. Hoe,
    *   and Markus P¨uschel'', "mapping \Pi_w"
    */
  def getMappingMatrix(streamWidth: Int): DenseMatrix[Int] = {
    val mappintMatrix =
      Array.tabulate(streamWidth, streamWidth) { (i, j) =>
        permuted.zipWithIndex.count { case (out, in) =>
          (out % streamWidth == j) && (in % streamWidth == i)
        }
      }
    new DenseMatrix(streamWidth, streamWidth, mappintMatrix.flatten)
  }

  override def toString: String = (0 until size).map(i => s"$i -> ${permuted(i)}").mkString("\n")
}

case class MatrixInterleave(row: Int, col: Int)
    extends Permutation(Array.tabulate(col, row)((i, j) => j * col + i).flatten)

object Permutation {

  def apply(permuted: Seq[Int]): Permutation = new Permutation(permuted)

  def identity(size: Int): Permutation = Permutation(0 until size)

  /** generate a random permutation of specific size
    */
  def random(size: Int): Permutation = Permutation(scala.util.Random.shuffle((0 until size).toList))

  def matrixInterleave(row: Int, col: Int): Permutation = Permutation(
    Array.tabulate(col, row)((i, j) => j * col + i).flatten
  )
}

class PermutationModule()