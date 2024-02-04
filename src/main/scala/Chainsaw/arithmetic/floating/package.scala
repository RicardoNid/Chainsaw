package Chainsaw.arithmetic

import Chainsaw.pow2
import spinal.core.sim._
import spinal.core._
import spinal.lib.experimental.math.Floating

package object floating {

  implicit class FloatingPointPimper(float: Floating) {
    def #=(value: Float): Unit = {
      val bitValue = java.lang.Float.floatToIntBits(value)
      float.sign     #= (bitValue >> 31) == 1
      float.exponent #= (bitValue >> 23) & 0xff
      float.mantissa #= bitValue & 0x7fffff
    }

    def #=(value: Double): Unit = { // FIXME: ....
      val bitValue = java.lang.Double.doubleToLongBits(value)
      float.sign     #= (bitValue >> 52) == 1
      float.exponent #= (bitValue >> 52) & 0x7ff
      float.mantissa #= bitValue & 0xfffffffffffffL
    }

    def toFloat: Float = {
      val bitValue = (float.sign.toBigInt << 31) + (float.exponent.toBigInt << 23) + float.mantissa.toBigInt
      java.lang.Float.intBitsToFloat(bitValue.toInt)
    }

    def toDouble: Double = {
      val bitValue = (float.sign.toBigInt << 63) + (float.exponent.toBigInt << 52) + float.mantissa.toBigInt
      java.lang.Double.longBitsToDouble(bitValue.toLong)
    }

    // only normal
    def randNormal(signed: Boolean): Unit = {
      float.exponentSize match {
        case 8 =>
          float.sign     #= (if (signed) scala.util.Random.nextBoolean() else false)
          float.exponent #= scala.util.Random.nextInt((pow2(8) - 2).toInt) + 1
          float.mantissa #= scala.util.Random.nextInt(pow2(23).toInt)
        case 11 =>
          float.sign     #= (if (signed) scala.util.Random.nextBoolean() else false)
          float.exponent #= scala.util.Random.nextInt((pow2(11) - 2).toInt) + 1
          float.mantissa #= BigInt(52, scala.util.Random)
      }
    }

    def randDenormal() = {
      float.exponentSize match {
        case 8 =>
          float.sign     #= scala.util.Random.nextBoolean()
          float.exponent #= 0
          float.mantissa #= (BigInt(23, util.Random) + 1) & (pow2(23) - 1)
        case 11 =>
          float.sign     #= scala.util.Random.nextBoolean()
          float.exponent #= 0
          float.mantissa #= (BigInt(52, util.Random) + 1) & (pow2(23) - 1)
      }
    }

    def isInf: Boolean = {
      float.exponentSize match {
        case 8  => (float.exponent.toBigInt == pow2(8) - 1) && (float.mantissa.toBigInt == 0)
        case 11 => (float.exponent.toBigInt == pow2(11) - 1) && (float.mantissa.toBigInt == 0)
      }
    }

    def isNaN: Boolean = {
      float.exponentSize match {
        case 8  => (float.exponent.toBigInt == pow2(8) - 1) && (float.mantissa.toBigInt != 0)
        case 11 => (float.exponent.toBigInt == pow2(11) - 1) && (float.mantissa.toBigInt != 0)
      }
    }
  }

  implicit class FloatingPointUtils(f: Floating) {
    def isNormalized = f.exponent =/= f.exponent.getZero && f.exponent =/= f.exponent.getAllTrue

    def isDenormalized = f.exponent === f.exponent.getZero

    def isSpecial = f.exponent === f.exponent.getAllTrue

    def significandValue = Mux(isNormalized, B(1, 1 bits) ## f.mantissa, B(0, 1 bits) ## f.mantissa).asUInt

    def exponentValue = Mux(isNormalized, f.exponent, B(1, f.exponentSize bits)).asUInt

    def isSinglePrecision: Boolean = f.exponentSize == 8 && f.mantissaSize == 23

    def isDoublePrecision: Boolean = f.exponentSize == 11 && f.mantissaSize == 52
  }

}
