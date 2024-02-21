package Chainsaw.algorithms

import org.scalatest.flatspec.AnyFlatSpecLike

class LinearPermutationTest extends AnyFlatSpecLike {

  "Linear Permutation" should "has correct factory methods" in {
    val n           = 3
    val bitReversal = LinearPermutation.bitReversal(n)
    println(s"${bitReversal.bitMatrix}\n$bitReversal")
    val perfectShuffle = LinearPermutation.perfectShuffle(n)
    println(s"${perfectShuffle.bitMatrix}\n$perfectShuffle")
    val stridePermutation = LinearPermutation.stridePermutation(n, 2)
    println(s"${stridePermutation.bitMatrix}\n$stridePermutation")
  }

  it should "behaves the same as the example in paper" in {

    // example in section 5 The Algorithm of the paper
    val permutation = LinearPermutation.stridePermutation(6, 2)
    permutation.solve(3)

  }

}
