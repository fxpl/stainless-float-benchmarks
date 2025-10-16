package stainless
package math

import stainless.annotation.*
import stainless.lang.*

private object LongHelpers {
  @library
  def compare(x: Long, y: Long): Int = if x < y then -1 else if x == y then 0 else 1

  @library
  def compareUnsigned(x: Long, y: Long): Int = math.wrapping(compare(x + Long.MinValue, y + Long.MinValue))
}

private object IntHelpers {
  @library
  def compare(x: Int, y: Int): Int = if x < y then -1 else if x == y then 0 else 1

  @library
  def compareUnsigned(x: Int, y: Int): Int = math.wrapping(compare(x + Int.MinValue, y + Int.MinValue))
}

object FdLibmBenchmark {

  // Constants used by multiple algorithms
  private val TWO24: Double = java.lang.Double.longBitsToDouble(0x4170000000000000L) // 1.67772160000000000000e+07
  private val TWO54: Double = java.lang.Double.longBitsToDouble(0x4350000000000000L) // 1.80143985094819840000e+16
  private val HUGE = 1.0e+300

  /*
   * Constants for bit-wise manipulation of IEEE 754 double
   * values. These constants are for the high-order 32-bits of a
   * 64-bit double value: 1 sign bit as the most significant bit,
   * followed by 11 exponent bits, and then the remaining bits as
   * the significand.
   */
  private val SIGN_BIT: Int = 0x8000_0000
  private val EXP_BITS: Int = 0x7ff0_0000
  private val EXP_SIGNIF_BITS: Int = 0x7fff_ffff

  /**
   * Return the low-order 32 bits of the double argument as an int.
   */
  @library
  private def __LO_CHECKED(x: Double): Int = {
    require(!x.isNaN)
    __LO(x)
  }

  @library
  private def __LO(x: Double): Int = {
    val transducer = java.lang.Double.doubleToLongBits(x)
    stainless.math.wrapping(transducer.toInt)
  }

  /**
   * Return a double with its low-order bits of the second argument
   * and the high-order bits of the first argument.
   */
  @library
  private def __LO_CHECKED(x: Double, low: Int): Double = {
    require(!x.isNaN)
    __LO(x, low)
  }//.ensuring(res => (x.isFinite == res.isFinite) && (x.isFinite ==> (x.isPositive == res.isPositive)) && (x.isFinite ==> (x.isNegative == res.isNegative)))


  @library
  private def __LO(x: Double, low: Int): Double = {
    val transX = java.lang.Double.doubleToLongBits(x)
    java.lang.Double.longBitsToDouble((transX & 0xFFFF_FFFF_0000_0000L) | (low & 0x0000_0000_FFFF_FFFFL))
  }//.ensuring(res => (x.isFinite == res.isFinite) && (x.isFinite ==> (x.isPositive == res.isPositive)) && (x.isFinite ==> (x.isNegative == res.isNegative)))

  /**
   * Return the high-order 32 bits of the double argument as an int.
   */
  @library
  private def __HI_CHECKED(x: Double): Int = {
    require(!x.isNaN)
    __HI(x)
  }

  @library
  private def __HI(x: Double): Int = {
    val transducer = java.lang.Double.doubleToLongBits(x)
    (transducer >> 32).toInt
  }

  /**
   * Return a double with its high-order bits of the second argument
   * and the low-order bits of the first argument..
   */
  @library
  private def __HI_CHECKED(x: Double, high: Int): Double = {
    require(!x.isNaN)
    __HI(x, high)
  }

  @library
  private def __HI(x: Double, high: Int): Double = {
    val transX = java.lang.Double.doubleToLongBits(x)
    java.lang.Double.longBitsToDouble((transX & 0x0000_0000_FFFF_FFFFL) | high.toLong << 32)
  }

  @library
  private def __HI_LO(high: Int, low: Int): Double = {
    java.lang.Double.longBitsToDouble((high.toLong << 32) | (low & 0xffff_ffffL))
  }

  object Atan {
    private val atanhi0 = java.lang.Double.longBitsToDouble(0x3fddac670561bb4fL) // atan(0.5)hi 4.63647609000806093515e-01
    private val atanhi1 = java.lang.Double.longBitsToDouble(0x3fe921fb54442d18L)
    private val atanhi2 = java.lang.Double.longBitsToDouble(0x3fef730bd281f69bL)
    private val atanhi3 = java.lang.Double.longBitsToDouble(0x3ff921fb54442d18L)

    private val atanlo0 = java.lang.Double.longBitsToDouble(0x3c7a2b7f222f65e2L) // atan(0.5)lo 2.26987774529616870924e-17
    private val atanlo1 = java.lang.Double.longBitsToDouble(0x3c81a62633145c07L)
    private val atanlo2 = java.lang.Double.longBitsToDouble(0x3c7007887af0cbbdL)
    private val atanlo3 = java.lang.Double.longBitsToDouble(0x3c91a62633145c07L)

    private val aT0 = java.lang.Double.longBitsToDouble(0x3fd555555555550dL)
    private val aT1 = java.lang.Double.longBitsToDouble(0xbfc999999998ebc4L)
    private val aT2 = java.lang.Double.longBitsToDouble(0x3fc24924920083ffL)
    private val aT3 = java.lang.Double.longBitsToDouble(0xbfbc71c6fe231671L)
    private val aT4 = java.lang.Double.longBitsToDouble(0x3fb745cdc54c206eL)
    private val aT5 = java.lang.Double.longBitsToDouble(0xbfb3b0f2af749a6dL)
    private val aT6 = java.lang.Double.longBitsToDouble(0x3fb10d66a0d03d51L)
    private val aT7 = java.lang.Double.longBitsToDouble(0xbfadde2d52defd9aL)
    private val aT8 = java.lang.Double.longBitsToDouble(0x3fa97b4b24760debL)
    private val aT9 = java.lang.Double.longBitsToDouble(0xbfa2b4442c6a6c2fL)
    private val aT10 = java.lang.Double.longBitsToDouble(0x3f90ad3ae322da11L)

    @opaque
    def computeAtan(x: Double): Double = {
      if x.isNaN then Double.NaN else
        val hx = __HI(x)
        val ix = hx & EXP_SIGNIF_BITS
        if ix >= 0x4410_0000 then // if |x| >= 2^66
          if ix > EXP_BITS || (ix == EXP_BITS && (__LO(x) != 0)) then x + x // NaN
          else if hx > 0 then atanhi3 + atanlo3
          else -atanhi3 - atanlo3
        else
          if ix < 0x3fdc_0000 && ix < 0x3e20_0000 && HUGE + x > 1.0 then x
          else
            val id =
              if ix < 0x3fdc_0000 then -1 else
                if ix < 0x3ff3_0000 then
                  if ix < 0x3fe60000 then 0 else 1
                else if ix < 0x4003_8000 then 2 else 3



            val absX = stainless.math.abs(x)
            val newX =
              if ix < 0x3fdc_0000 then x else
                if ix < 0x3ff3_0000 then
                  if ix < 0x3fe60000 then (2.0 * absX - 1.0) / (2.0 + absX) else (absX - 1.0) / (absX + 1.0)
                else if ix < 0x4003_8000 then (absX - 1.5) / (1.0 + 1.5 * absX) else -1.0 / absX

            // end of argument reduction
            val z = newX * newX
            val w = z * z
            // break sum from i=0 to 10 aT[i]z**(i+1) into odd and even poly
            val s1 = z * (aT0 + w * (aT2 + w * (aT4 + w * (aT6 + w * (aT8 + w * aT10)))))
            val s2 = w * (aT1 + w * (aT3 + w * (aT5 + w * (aT7 + w * aT9))))
            if id < 0 then newX - newX * (s1 + s2)
            else
              val atanhiId = id match
                case 0 => atanhi0
                case 1 => atanhi1
                case 2 => atanhi2
                case 3 => atanhi3

              val atanloId = id match
                case 0 => atanlo0
                case 1 => atanlo1
                case 2 => atanlo2
                case 3 => atanlo3
              val z = atanhiId - ((newX * (s1 + s2) - atanloId) - newX)
              if hx < 0 then -z else z

    }.ensuring( res =>
      (x.isNaN == res.isNaN)
      && (x.isPositive == res.isPositive)
      && (x.isNegative == res.isNegative)
      && (x.isZero == res.isZero)
      && (!x.isNaN == (!res.isNaN && (- Pi / 2 <= res && res <= Pi / 2)))
      && (x.isPosInfinity ==> (res == Pi / 2))
      && (x.isNegInfinity ==> (res == -Pi / 2))
    )
  }
}
