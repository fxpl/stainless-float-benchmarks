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

  object Hypot {
    private val TWO_MINUS_600: Double = java.lang.Double.longBitsToDouble(0x1a70000000000000L)
    private val TWO_PLUS_600: Double = java.lang.Double.longBitsToDouble(0x6570000000000000L)

    @opaque
    def computeHypot(x: Double, y: Double): Double = {
      val a = stainless.math.abs(x)
      val b = stainless.math.abs(y)
      if !a.isFinite || !b.isFinite then
        if a.isPosInfinity || b.isPosInfinity then Double.PositiveInfinity else a + b // Propagate NaN significand bits
      else
        var big = if b > a then b else a
        var small = if b > a then a else b
        var hbig = __HI(big) // high word of a
        var hsmall = __HI(small) // high word of b

        if hbig - hsmall > 0x3c00000 then big + small // x / y > 2**60
        else
          val cond1 = big > java.lang.Double.longBitsToDouble(0x5f300000ffffffffL)
          val k: Int = if cond1 then 600 else 0
          val hbig2 = if cond1 then hbig - 0x25800000 else hbig
          val hsmall2 = if cond1 then hsmall - 0x25800000 else hsmall
          val big2 = if cond1 then big * TWO_MINUS_600 else big
          val small2 = if cond1 then small * TWO_MINUS_600 else small

          if small == 0 then big
          else
            val cond2 = small2 < java.lang.Double.longBitsToDouble(0x20b0000000000000L) // small < 2**-500
            val cond3 = small2 < stainless.DoubleConsts.MIN_NORMAL // subnormal b or 0
            val t1 = if cond2 && cond3 then java.lang.Double.longBitsToDouble(0x7fd0000000000000L) else 0.0
            val small3 = if cond2 then if cond3 then small2 * t1 else small2 * TWO_PLUS_600 else small2
            val big3 = if cond2 then if cond3 then big2 * t1 else big2 * TWO_PLUS_600 else big2
            val k2 = if cond2 then if cond3 then k - 1022 else k - 600 else k
            val hbig3 = if cond2 then if cond3 then hbig2 else hbig2 + 0x25800000 else hbig2
            val hsmall3 = if cond2 then if cond3 then hsmall2 else hsmall2 + 0x25800000 else hsmall2

            val w: Double = big3 - small3

            val cond4 = w > small3
            val t11 = if cond4 then __HI(0.0, hbig3) else __HI(0.0, hbig3 + 0x00100000)
            val big4 = if cond4 then big3 else big3 + big3
            val t2 = big4 - t11
            val y1 = if cond4 then 0.0 else __HI(0.0, hsmall3)
            val y2 = if cond4 then 0.0 else small3 - y1
            val w2 =
              if cond4 then stainless.math.sqrt(t11 * t11 - (small3 * (-small3) - t2 * (big4 + t11)))
              else stainless.math.sqrt(t11 * y1 - (w * (-w) - (t11 * y2 + t2 * small3)))

            if k2 != 0 then stainless.math.powerOfTwoD(k2) * w2 else w2

    }.ensuring(res =>
      ((x.isInfinity || y.isInfinity) ==> res.isPosInfinity) &&
      (res.isNaN == (!x.isInfinity && !y.isInfinity && (x.isNaN || y.isNaN))) &&
      (!res.isNaN == res.isPositive) &&
      ((x.isZero && !y.isNaN) ==> (res == stainless.math.abs(y))) &&
      ((y.isZero && !x.isNaN) ==> (res == stainless.math.abs(x)))
    )
  }
}
