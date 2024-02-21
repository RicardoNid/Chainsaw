package Chainsaw

import scala.language.implicitConversions

package object algorithms {

  // upgrade transform and transform step to transform list for transform 'arithmetics'
  implicit def transform2Step[T](transform: Transform[T]): TransformStep[T] = new TransformStep(Seq(transform))
  implicit def transform2List[T](transform: Transform[T]): TransformList[T] = new TransformList(
    Seq(new TransformStep(Seq(transform)))
  )
  implicit def step2List[T](step: TransformStep[T]): TransformList[T] = new TransformList(Seq(step))

  // implicit numeric instance for custom numeric types

  // providing a field instance for GF2
  implicit val bitNumeric: Field[GF2] = GF2(false)

  // providing definitions to treat Double as Real(a Field type)
  trait DoubleAsReal extends Field[Double] {
    def plus(x: Double, y: Double): Double   = x + y
    def minus(x: Double, y: Double): Double  = x - y
    def times(x: Double, y: Double): Double  = x * y
    def divide(x: Double, y: Double): Double = x / y
    def negate(x: Double): Double            = -x
    def fromInt(x: Int): Double              = x.toDouble
    def toInt(x: Double): Int                = x.toInt
    def toLong(x: Double): Long              = x.toLong
    def toFloat(x: Double): Float            = x.toFloat
    def toDouble(x: Double): Double          = x
    // logic in Numeric base trait mishandles abs(-0.0)
    override def abs(x: Double): Double = math.abs(x)
  }

  // providing a field instance for Double
  implicit object DoubleAsReal extends DoubleAsReal with Ordering.DoubleOrdering
}
