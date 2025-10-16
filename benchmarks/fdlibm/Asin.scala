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

  object Asin {
    private val pio2_hi = java.lang.Double.longBitsToDouble(0x3ff921fb54442d18L)
    private val pio2_lo = java.lang.Double.longBitsToDouble(0x3c91a62633145c07L)
    private val pio4_hi = java.lang.Double.longBitsToDouble(0x3fe921fb54442d18L)
    private val pS0 = java.lang.Double.longBitsToDouble(0x3fc5555555555555L)
    private val pS1 = java.lang.Double.longBitsToDouble(0xbfd4d61203eb6f7dL)
    private val pS2 = java.lang.Double.longBitsToDouble(0x3fc9c1550e884455L)
    private val pS3 = java.lang.Double.longBitsToDouble(0xbfa48228b5688f3bL)
    private val pS4 = java.lang.Double.longBitsToDouble(0x3f49efe07501b288L)
    private val pS5 = java.lang.Double.longBitsToDouble(0x3f023de10dfdf709L)
    private val qS1 = java.lang.Double.longBitsToDouble(0xc0033a271c8a2d4bL)
    private val qS2 = java.lang.Double.longBitsToDouble(0x40002ae59c598ac8L)
    private val qS3 = java.lang.Double.longBitsToDouble(0xbfe6066c1b8d0159L)
    private val qS4 = java.lang.Double.longBitsToDouble(0x3fb3b8c5b12e9282L)

    @opaque
    def computeAsin(x: Double): Double = {
      if x.isNaN then Double.NaN
      else
        val hx = __HI_CHECKED(x)
        val ix = hx & EXP_SIGNIF_BITS
        if ix >= 0x3ff0_0000 then // |x| >= 1
          if ((ix - 0x3ff0_0000) | __LO(x)) == 0 then x * pio2_hi + x * pio2_lo // asin(1) = +-pi/2 with inexact
          else (x - x) / (x - x) // asin(|x| > 1) is NaN
        else if ix < 0x3fe0_0000 then // |x| < 0.5
          if ix < 0x3e40_0000 && HUGE + x > 1.0 then x // return x with inexact if x != 0
          else
            val t = if ix < 0x3e40_0000 then 0 else x * x
            val p = t * (pS0 + t * (pS1 + t * (pS2 + t * (pS3 + t * (pS4 + t * pS5)))))
            val q = 1.0 + t * (qS1 + t * (qS2 + t * (qS3 + t * qS4)))
            val w = p / q
            x + x * w
        else
          // 1 > |x| >= 0.5
          val t = (1.0 - stainless.math.abs(x)) * 0.5
          val p = t * (pS0 + t * (pS1 + t * (pS2 + t * (pS3 + t * (pS4 + t * pS5)))))
          val q = 1.0 + t * (qS1 + t * (qS2 + t * (qS3 + t * qS4)))
          val s = stainless.math.sqrt(t)
          val w = if ix >= 0x3FEF_3333 then p / q else __LO(s, 0)
          val p2 = if ix >= 0x3FEF_3333 then p else 2.0 * s * (p / q) - (pio2_lo - 2.0 * ((t - w * w) / (s + w)))
          val q2 = if ix >= 0x3FEF_3333 then q else pio4_hi - 2.0 * w
          val t2 = if ix >= 0x3FEF_3333 then pio2_hi - (2.0*(s + s*w) - pio2_lo) else pio4_hi - (p2 - q2)
          if hx > 0 then t2 else -t2
    }.ensuring(res =>
      ((x.isNaN || x < -1.0d || x > 1.0d) == res.isNaN)
        && ((x.isPositive && x.isZero) == (res.isPositive && res.isZero))
        && ((x.isNegative && x.isZero) == (res.isNegative && res.isZero))
        && ((x.isFinite && x == -1.0d) == (res.isFinite && res == -stainless.math.Pi / 2))
        && ((x.isFinite && x == 1.0d) == (res.isFinite && res == stainless.math.Pi / 2))
        && ((x.isFinite && -1.0d <= x && x <= 1.0d) == (res.isFinite && -stainless.math.Pi / 2 <= res && res <= stainless.math.Pi / 2))
    )
  }
}
