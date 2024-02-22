package Chainsaw.algorithms

import breeze.math.Complex

object FindPeak {

  def getTransform(n: Int): TransformList[Complex] = {
    val fft: TransformList[Complex] = Fft.getFft(inverse = false, Fft.getFactorsForUnrolled(n))
    val cmp                         = (a: Complex, b: Complex) => if (a.abs > b.abs) 1 else -1
    val sort                        = Bitonic.getNetwork(n, false, cmp)
    fft * sort
  }

  def main(args: Array[String]): Unit = {
    val n         = 64
    val transform = getTransform(n)
    println(transform)
    println(transform.printFolded(8))
    // TODO: try to merge neighboring SP modules
  }

}
