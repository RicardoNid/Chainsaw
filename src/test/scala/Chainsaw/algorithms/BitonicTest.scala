package Chainsaw.algorithms

import org.scalatest.flatspec.AnyFlatSpecLike

class BitonicTest extends AnyFlatSpecLike {

  def testOnce(n: Int, sort: Seq[Int] => Seq[Int]) = {
    val data   = Permutation.random(n).permuted
    val sorted = sort(data)
    assert(sorted == sorted.sorted)
  }
  val n       = 32
  val cmp = (a: Int, b: Int) => a - b
  val transform = Bitonic.getNetwork(n, ascending = true, cmp)
  val recursiveSort: Seq[Int] => Seq[Int] = transform.transform(_)

  "bitonic sort" should "work" in (1 to 10).foreach(_ => testOnce(n, recursiveSort))
  it should "print" in transform.printSteps()
  it should "fold" in {
    transform.printFolded(n / 2)
    transform.printFolded(n / 4)
  }

}
