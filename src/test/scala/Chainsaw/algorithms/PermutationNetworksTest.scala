package Chainsaw.algorithms

import org.scalatest.flatspec.AnyFlatSpec

class PermutationNetworksTest extends AnyFlatSpec {

  val testSize = 32

  "Permutation Network object" should "find the simplest network for you" in {}

  "Benes Network" should "generate control bits according to your permutation" in {

    val permutation = Permutation.random[Int](testSize)
    val control     = Benes.getBenesControl(permutation)
    println(control.map(_.map(if (_) 1 else 0).mkString(" ")).mkString("\n"))

  }

  it should "work" in {

    val permutation = Permutation.random[Int](testSize)
    val benes       = Benes.getBenes(permutation)
    val yours       = benes.transform(0 until testSize)
    val golden      = permutation.permuted
    assert(yours.equals(golden))
    benes.printSteps()

  }


}
