package Chainsaw.algorithms

// TODO: implement GF(m) and GF(2^m)

/** define GF(2) operations by a numeric type
  * @param value
  *   0 or 1 represented by a boolean value
  */
case class GF2(value: Boolean) extends Field[GF2] {

  override def plus(x: GF2, y: GF2): GF2 = GF2(x.value ^ y.value)

  override def minus(x: GF2, y: GF2): GF2 = GF2(x.value ^ y.value)

  override def times(x: GF2, y: GF2): GF2 = GF2(x.value & y.value)

  override def divide(x: GF2, y: GF2): GF2 = {
    require(y != GF2(0), "division by zero")
    GF2(x.value)
  }

  override def negate(x: GF2): GF2 = GF2(x.value) // FIXME: return "this" will fail the test

  override def fromInt(x: Int): GF2 = GF2(x % 2 == 1)

  override def toInt(x: GF2): Int = if (x.value) 1 else 0

  override def toLong(x: GF2): Long = if (x.value) 1L else 0L

  override def toFloat(x: GF2): Float = if (x.value) 1f else 0f

  override def toDouble(x: GF2): Double = if (x.value) 1d else 0d

  /** we define 1 > 0 for sorting
    */
  override def compare(x: GF2, y: GF2): Int = if (x.value == y.value) 0 else if (x.value) 1 else -1

  override def toString: String = if (value) "1" else "0"

}

object GF2 {
  def apply(value: Int): GF2 = new GF2(value % 2 == 1)
}
