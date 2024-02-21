package Chainsaw.algorithms

import scala.language.implicitConversions

/** A trait that represents a field, including division operation, for a type `T`.
  *
  * @tparam T
  *   the type of the elements in the field
 * @example for usage of this trait, see [[Chainsaw.algorithms.Matrix]]
  */
trait Field[T] extends Numeric[T] {

  def divide(x: T, y: T): T
  class Ops(lhs: T) {
    def /(rhs: T): T = divide(lhs, rhs)
  }
  implicit def mkFieldOps(lhs: T): Ops = new Ops(lhs)
}
