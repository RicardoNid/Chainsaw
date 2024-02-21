package Chainsaw.algorithms

import org.scalatest.flatspec.AnyFlatSpecLike
import breeze.linalg._
import scala.util.Random

class MatrixTest extends AnyFlatSpecLike {

  def same(a: Double, b: Double): Boolean = math.abs(a - b) < 1e-6
  def same(a: Matrix[Double], b: DenseMatrix[Double]): Boolean = {
    a.transpose.data.flatten.zip(b.toArray).forall(pair => same(pair._1, pair._2))
  }
  def same(a: Matrix[Double], b: Matrix[Double]): Boolean = {
    a.data.zip(b.data).forall(rows => rows._1.zip(rows._2).forall(pair => same(pair._1, pair._2)))
  }

  // by testing the matrix operations with Double, we confirm the correctness on other numeric types
  "Matrix numeric operations" should "be correct" in {
    def testOperations(n: Int) = {
      val value0        = Array.tabulate(n, n)((_, _) => Random.nextDouble())
      val value1        = Array.tabulate(n, n)((_, _) => Random.nextDouble())
      val myMatrix0     = Matrix(value0)
      val myMatrix1     = Matrix(value1)
      val breezeMatrix0 = DenseMatrix(value0: _*)
      val breezeMatrix1 = DenseMatrix(value1: _*)
      assert(same(myMatrix0.det, det(breezeMatrix0)), "bad determinant")
      assert(same(myMatrix0.transpose, breezeMatrix0.t), "bad transpose")
      assert(same(myMatrix0 + myMatrix1, breezeMatrix0 + breezeMatrix1), "bad addition")
      assert(same(myMatrix0 - myMatrix1, breezeMatrix0 - breezeMatrix1), "bad subtraction")
      assert(same(myMatrix0 * myMatrix1, breezeMatrix0 * breezeMatrix1), "bad multiplication")
      assert(same(myMatrix0 ^ 1, breezeMatrix0), "bad power")
      assert(same(myMatrix0 ^ 2, breezeMatrix0 * breezeMatrix0), "bad power")

      // TODO: gaussian elimination, rank, svd, lu, qr, etc.
    }
    (0 until 100).foreach(_ => testOperations(3))
  }

  def parseMatrix(s: String): Matrix[Double] = {
    val elements = s.split(" ").map(_.toDouble)
    val rowCount = elements(0).toInt
    val colCount = elements(1).toInt
    require(elements.length == rowCount * colCount + 2, "bad input")
    val data = elements.drop(2).grouped(colCount).toArray
    Matrix(data)
  }

  def parseGf2Matrix(s: String): Matrix[GF2] = {
    val elements = s.split(" ").map(_.toInt)
    val rowCount = elements(0)
    val colCount = elements(1)
    require(elements.length == rowCount * colCount + 2, "bad input")
    val data = elements.drop(2).map(GF2(_)).grouped(colCount).toArray
    Matrix(data)
  }

  // TODO: more tests on isRef and isRref; isConsistent and isUnique

  it should "pass all examples in 1.2" in {
    val example3  = parseMatrix("3 6 0 3 -6 6 4 -5 3 -7 8 -5 8 9 3 -9 12 -9 6 15")
    val solution3 = parseMatrix("3 6 1 0 -2 3 0 -24 0 1 -2 2 0 -7 0 0 0 0 1 4")
    assert(same(example3.getRref, solution3), "bad rref")
    val example5 = parseMatrix("3 5 0 3 -6 6 4 3 -7 8 -5 8 3 -9 12 -9 6")
    val target5  = parseMatrix("3 1 -5 9 15")
    example5.solve(target5)
  }

  it should "pass all examples in 2.2" in {
    val example7 = parseMatrix("3 3 0 1 2 1 0 3 4 -3 8")
    assert(same(example7 * example7.getInverse, Matrix.identity[Double](3)), "bad inverse")
  }

  it should "pass all examples in 2.5" in {
    val example2 = parseMatrix("4 5 2 4 -1 5 -2 -4 -5 3 -8 1 2 -5 -4 1 8 -6 0 7 -3 1")
    val targetL  = parseMatrix("4 4 1 0 0 0 -2 1 0 0 1 -3 1 0 -3 4 2 1")
    val (l, u)   = example2.getLU
    assert(u.isRef)
    assert(same(l, targetL))
    assert(same(l * u, example2))
  }

  implicit val divGf2: (GF2, GF2) => GF2 = (a, b) => {
    require(b != GF2(0))
    a
  }

  "GF2" should "have matrix operations" in {
    // TODO: verify the correctness of GF2 matrix operations
    val n         = 3
    val value0    = Array.tabulate(n, n)((_, _) => Random.nextInt(2))
    val value1    = Array.tabulate(n, n)((_, _) => Random.nextInt(2))
    val myMatrix0 = Matrix(value0.map(_.map(GF2(_))))
    val myMatrix1 = Matrix(value1.map(_.map(GF2(_))))
    myMatrix0.det
    myMatrix0.transpose
    myMatrix0 + myMatrix1
    myMatrix0 - myMatrix1
    myMatrix0 * myMatrix1
    val example = parseGf2Matrix("3 3 1 1 0 1 0 0 0 1 0")
    println(example)
    println(example.getRef)
    println(example.getRref)
    val identity = parseGf2Matrix("3 3 1 0 0 0 1 0 0 0 1")
    val target   = parseGf2Matrix("3 1 1 0 1")
    identity.solve(target)
    val all = Array(GF2(0), GF2(1))
    println("image")
    println(identity.getImage(all).mkString("\n"))
    println("kernel")
    println(identity.getKernel(all).mkString("\n"))
  }

  it should "help to represent linear permutations" in {
    val column6 = Matrix.column(GF2(1), GF2(1), GF2(0))
    val bitReversal = Matrix(
      Array(
        Array(0, 0, 1),
        Array(0, 1, 0),
        Array(1, 0, 0)
      ).map(_.map(GF2(_)))
    )

    println(column6)
    println(bitReversal * column6)
  }

}
