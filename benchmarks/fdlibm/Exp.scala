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

  object Exp {
    private val half: Array[Double] = Array(0.5, -0.5)
    private val half0: Double = 0.5
    private val half1: Double = -0.5
    private val huge: Double = 1.0e+300
    private val twom1000: Double = java.lang.Double.longBitsToDouble(0x170000000000000L)
    private val o_threshold: Double = java.lang.Double.longBitsToDouble(0x40862e42fefa39efL)
    private val u_threshold: Double = java.lang.Double.longBitsToDouble(0xc0874910d52d3051L)
    private val ln2HI = Array(java.lang.Double.longBitsToDouble(0x3fe62e42fee00000L), java.lang.Double.longBitsToDouble(0xbfe62e42fee00000L))
    private val ln2HI0: Double = java.lang.Double.longBitsToDouble(0x3fe62e42fee00000L)
    private val ln2HI1: Double = java.lang.Double.longBitsToDouble(0xbfe62e42fee00000L)
    private val ln2LO = Array(java.lang.Double.longBitsToDouble(0x3dea39ef35793c76L), java.lang.Double.longBitsToDouble(0xbdea39ef35793c76L))
    private val ln2LO0 = java.lang.Double.longBitsToDouble(0x3dea39ef35793c76L)
    private val ln2LO1 = java.lang.Double.longBitsToDouble(0xbdea39ef35793c76L)

    private val invln2 = java.lang.Double.longBitsToDouble(0x3ff71547652b82feL)
    private val P1 = java.lang.Double.longBitsToDouble(0x3fc555555555553eL)
    private val P2 = java.lang.Double.longBitsToDouble(0xbf66c16c16bebd93L)
    private val P3 = java.lang.Double.longBitsToDouble(0x3f11566aaf25de2cL)
    private val P4 = java.lang.Double.longBitsToDouble(0xbebbbd41c5d26bf1L)
    private val P5 = java.lang.Double.longBitsToDouble(0x3e66376972bea4d0L)

    @opaque
    def computeExp(x: Double): Double = {

      if x.isNaN then Double.NaN else
        val hx: Int = __HI_CHECKED(x)
        /* high word of x */
        val xsb: Int = (hx >> 31) & 1
        /* sign bit of x */
        val hx2: Int = hx & EXP_SIGNIF_BITS /* high word of |x| */

        /* filter out non-finite argument */
        if hx2 >= 0x40862E42 && (hx2 >= 0x7ff00000 || x > o_threshold || x < u_threshold) then
          /* if |x| >= 709.78... */
          if hx2 >= 0x7ff00000 then
            if ((hx2 & 0xfffff) | __LO_CHECKED(x)) != 0 then x + x /* NaN */
            else if xsb == 0 then x else 0.0 /* exp(+-inf) = {inf, 0} */
          else if (x > o_threshold) huge * huge /* overflow */
          else twom1000 * twom1000 /* underflow */
        else if !(hx2 > 0x3fd62e42) && hx2 < 0x3e300000 && huge + x > 1.0 then 1.0 + x
        else
          val halfXsb = if xsb == 0 then half0 else half1
          val ln2HIXsb = if xsb == 0 then ln2HI0 else ln2HI1
          val ln2LOXsb = if xsb == 0 then ln2LO0 else ln2LO1
          val k: Int = if hx2 > 0x3fd62e42 then if hx2 < 0x3FF0A2B2 then 1 - xsb - xsb else (invln2 * x + halfXsb).toInt else 0
          val hi: Double = if hx2 > 0x3fd62e42 then if hx2 < 0x3FF0A2B2 then x - ln2HIXsb else x - k * ln2HI0 else 0.0
          val lo: Double = if hx2 > 0x3fd62e42 then if hx2 < 0x3FF0A2B2 then ln2LOXsb else k * ln2LO0 else 0.0
          val newX: Double = if hx2 > 0x3fd62e42 then hi - lo else x

          /* x is now in primary range */
          val t: Double = newX * newX
          val c: Double = newX - t * (P1 + t * (P2 + t * (P3 + t * (P4 + t * P5))))
          if k == 0 then 1.0 - ((newX * c) / (c - 2.0) - newX)
          else
            val y: Double = 1.0 - ((lo - (newX * c) / (2.0 - c)) - hi)
            if k >= -1021 then __HI_CHECKED(y, __HI_CHECKED(y) + (k << 20)) /* add k to y's exponent */
            else __HI_CHECKED(y, __HI_CHECKED(y) + ((k + 1000) << 20)) * twom1000
    }.ensuring(res =>
      res.isNaN == x.isNaN
      && (x.isPosInfinity ==> res.isPosInfinity)
      && (x.isNegInfinity ==> (res.isZero && res.isPositive))
      && (x.isZero ==> (res == 1))
      && ((!x.isNaN && x.isPositive) ==> res >= 1) // Converse does not hold: x = -7.458340731200206E-155
      && ((!x.isNaN && x.isNegative) ==> (res.isPositive && res <= 1))
    )
  }
}
