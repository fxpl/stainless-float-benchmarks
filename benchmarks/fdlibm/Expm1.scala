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

  object Expm1 {
    private val huge: Double = 1.0e+300
    private val tiny: Double = 1.0e-300
    private val o_threshold: Double = java.lang.Double.longBitsToDouble(0x40862e42fefa39efL)
    private val u_threshold: Double = java.lang.Double.longBitsToDouble(0xc0874910d52d3051L)
    private val ln2_hi: Double = java.lang.Double.longBitsToDouble(0x3fe62e42fee00000L)
    private val ln2_lo = java.lang.Double.longBitsToDouble(0x3dea39ef35793c76L)
    private val invln2 = java.lang.Double.longBitsToDouble(0x3ff71547652b82feL)

    private val Q1 = java.lang.Double.longBitsToDouble(0xbfa11111111110f4L)
    private val Q2 = java.lang.Double.longBitsToDouble(0x3f5a01a019fe5585L)
    private val Q3 = java.lang.Double.longBitsToDouble(0xbf14ce199eaadbb7L)
    private val Q4 = java.lang.Double.longBitsToDouble(0x3ed0cfca86e65239L)
    private val Q5 = java.lang.Double.longBitsToDouble(0xbe8afdb76e09c32dL)

    @opaque
    def computeExpm1(x: Double): Double = {
      if x.isNaN then Double.NaN else
        val hx = __HI_CHECKED(x) // high word of x
        val xsb = hx & SIGN_BIT // sign bit of x
        val y = stainless.math.abs(x)
        val hx2: Int = hx & EXP_SIGNIF_BITS /* high word of |x| */


        /* filter out non-finite argument */
        if hx2 >= 0x4043_687A && ((hx >= 0x4086_2E42 && (hx2 >= 0x7ff00000 || x > o_threshold)) || (xsb != 0 && x + tiny < 0.0)) then
          if hx >= 0x4086_2E42 then
            /* if |x| >= 709.78... */
            if hx2 >= 0x7ff00000 then
              if ((hx2 & 0xfffff) | __LO_CHECKED(x)) != 0 then x + x /* NaN */
              else if xsb == 0 then x else -1.0 /* exp(+-inf) = {inf, 0} */
            else huge * huge /* overflow */
          else tiny - 1.0
        else
          if !(hx2 > 0x3fd6_2e42) && hx2 < 0x3c90_0000 then x - ((huge + x) - (huge + x))
          else
            // argument reduction
            val k: Int = if hx2 > 0x3fd6_2e42 then if hx2 < 0x3FF0_A2B2 then if xsb == 0 then 1 else -1 else (invln2 * x + (if xsb == 0 then 0.5 else -0.5)).toInt else 0
            val hi: Double = if hx2 > 0x3fd6_2e42 then if hx2 < 0x3FF0_A2B2 then if xsb == 0 then x - ln2_hi else x + ln2_hi else x - k * ln2_hi else 0.0
            val lo: Double = if hx2 > 0x3fd6_2e42 then if hx2 < 0x3FF0_A2B2 then if xsb == 0 then ln2_lo else -ln2_lo else k * ln2_lo else 0.0
            val x2: Double = if hx2 > 0x3fd6_2e42 then hi - lo else x
            val c: Double = if hx2 > 0x3fd6_2e42 then (hi - x2) - lo else 0

            // x is now in primary range
            val hfx = 0.5 * x2
            val hxs = x2 * hfx
            val r1 = 1.0 + hxs * (Q1 + hxs * (Q2 + hxs * (Q3 + hxs * (Q4 + hxs * Q5))))
            val t = 3.0 - r1 * hfx
            val eInit = hxs * ((r1 - t) / (6.0 - x2 * t))
            if k == 0 then
              x2 - (x2 * eInit - hxs) // c is 0
            else
              val e = (x2 * (eInit - c) - c) - hxs
              if k == -1 then 0.5 * (x2 - e) - 0.5
              else if k == 1 then
                if x2 < -0.25 then -2.0 * (e - (x2 + 0.5)) else 1.0 + 2.0 * (x2 - e)
              else if k <= -2 || k > 56 then // suffice to return exp(x) - 1
                val y = 1.0 - (e - x2)
                __HI_CHECKED(y, __HI_CHECKED(y) + (k << 20)) - 1.0 // add k to y's exponent
              else
                if k < 20 then
                  val t = __HI_CHECKED(1.0, 0x3ff0_0000 - (0x2_00000 >> k)) // t = 1-2^-k
                  val y = t - (e - x2)
                  __HI_CHECKED(y, __HI_CHECKED(y) + (k << 20)) // add k to y's exponent
                else
                  val t = __HI_CHECKED(1.0, ((0x3ff - k) << 20)) // 2^-k
                  val y = (x2 - (e + t)) + 1
                  __HI_CHECKED(y, __HI_CHECKED(y) + (k << 20)) // add k to y's exponent
    }.ensuring(res =>
      (res.isNaN == x.isNaN)
        && (x.isPosInfinity ==> res.isPosInfinity)
        && (x.isNegInfinity ==> (res == -1))
        && (x.isZero ==> (res == 0))
        && (x.isPositive == res.isPositive)
        && ((!x.isNaN) ==> (-1 <= res))
    )
  }
}
