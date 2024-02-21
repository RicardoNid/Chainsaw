package Chainsaw.algorithms

import org.scalatest.flatspec.AnyFlatSpecLike

class PermutationTest extends AnyFlatSpecLike {

  "Permutation" should "print correct matrix" in {
    val n        = 4
    val identity = Permutation.identity(n)
    println(s"${identity}\n${identity.getPermutationMatrix}")
    val random = Permutation.random(n)
    println(s"${random}\n${random.getPermutationMatrix}")

  }

}
