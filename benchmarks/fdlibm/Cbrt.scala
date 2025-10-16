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

  object Cbrt {
    // unsigned
    private val B1 = 715094163
    private val B2 = 696219795
    private val C = 19.0/35.0
    private val D = -864.0/1225.0
    private val E = 99.0/70.0
    private val F = 45.0/28.0
    private val G = 5.0/14.0

    @opaque
    def computeCbrt(x: Double): Double = {
      if !x.isFinite || x == 0.0  then x // Handles signed zeros properly
      else
        val sign = if x < 0.0 then -1.0 else 1.0
        val x_abs = stainless.math.abs(x) // x <- |x|

        // Rough cbrt to 5 bits
        val t =
          if x_abs < java.lang.Double.longBitsToDouble(0x10000000000000L) then
            // subnormal number
            val temp_t = 18014398509481984.0 * x_abs
            __HI_CHECKED(temp_t, __HI_CHECKED(temp_t) / 3 + B2)
          else
            val hx = __HI_CHECKED(x_abs) // high word of x
            __HI_CHECKED(0.0, hx / 3 + B1)

        // New cbrt to 23 bits, may be implemented in single precision
        val r = t * t / x_abs
        val s = C + r * t
        // Chopped to 20 bits and make it larger than cbrt(x)
        val t2 = __LO_CHECKED(t * (G + F / (s + E + D / s)), 0)
        val t3 = __HI_CHECKED(t2, __HI_CHECKED(t2) + 0x00000001)
        // One step newton iteration to 53 bits with error less than 0.667 ulps
        val r2 = x_abs / (t3 * t3)
        val w = t3 + t3
        // Restore the original sign bit
        sign * (t3 + t3 * ((r2 - t3) / (w + r2)))
      }.ensuring(res =>
        (res.isNaN == x.isNaN) &&
        (x.isPosInfinity ==> res.isPosInfinity) &&
        (x.isNegInfinity ==> res.isNegInfinity) &&
        (x.isZero == res.isZero) &&
        ((!x.isNaN && x == 1) ==> (res == 1)) &&
        ((!x.isNaN && x == -1) ==> (res == -1)) &&
        (x.isPositive == res.isPositive) &&
        (x.isNegative == res.isNegative) &&
        ((x.isFinite && stainless.math.abs(x) > 1) ==> stainless.math.abs(res) < stainless.math.abs(x)) &&
        ((x.isFinite && !x.isZero && stainless.math.abs(x) < 1) ==> stainless.math.abs(res) > stainless.math.abs(x))
      )
  }
}
