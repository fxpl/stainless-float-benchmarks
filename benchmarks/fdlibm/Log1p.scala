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

  object Log1p {
    private val ln2_hi = java.lang.Double.longBitsToDouble(0x3fe62e42fee00000L) // 6.93147180369123816490e-01
    private val ln2_lo = java.lang.Double.longBitsToDouble(0x3dea39ef35793c76L) // 1.90821492927058770002e-10
    private val Lp1 = java.lang.Double.longBitsToDouble(0x3fe5555555555593L) // 6.666666666666735130e-01
    private val Lp2 = java.lang.Double.longBitsToDouble(0x3fd999999997fa04L) // 3.999999999940941908e-01
    private val Lp3 = java.lang.Double.longBitsToDouble(0x3fd2492494229359L) // 2.857142874366239149e-01
    private val Lp4 = java.lang.Double.longBitsToDouble(0x3fcc71c51d8e78afL) // 2.222219843214978396e-01
    private val Lp5 = java.lang.Double.longBitsToDouble(0x3fc7466496cb03deL) // 1.818357216161805012e-01
    private val Lp6 = java.lang.Double.longBitsToDouble(0x3fc39a09d078c69fL) // 1.531383769920937332e-01
    private val Lp7 = java.lang.Double.longBitsToDouble(0x3fc2f112df3e5244L) // 1.479819860511658591e-01

    @opaque
    def computeLog1p(x: Double): Double = {
      if x.isNaN then Double.NaN
      else
        val hx: Int = __HI_CHECKED(x) /* high word of x */
        val ax: Int = hx & EXP_SIGNIF_BITS
        if hx < 0x3FDA_827A && (ax >= 0x3ff0_0000 || ax < 0x3e20_0000) then
          /* x < 0.41422  */
          if ax >= 0x3ff0_0000 then
            /* x <= -1.0 */
            if x == -1.0 /* log1p(-1)=-inf */ then Double.NegativeInfinity
            else  Double.NaN /* log1p(x < -1) = NaN */
          else
            /* |x| < 2**-29 */
            if TWO54 + x > 0.0 /* raise inexact */ && ax < 0x3c90_0000 /* |x| < 2**-54 */ then x
            else  x - x * x * 0.5
        else
          val k: Int = if hx < 0x3FDA_827A && (hx > 0 || hx <= 0xbfd2_bec3) then 0 else 1
          val f: Double = if hx < 0x3FDA_827A && (hx > 0 || hx <= 0xbfd2_bec3) then x else 0.0
          val huInit: Int = if hx < 0x3FDA_827A && (hx > 0 || hx <= 0xbfd2_bec3) then 1 else 0

          if hx >= EXP_BITS then x + x
          else
            val u: Double = if k != 0 then if hx < 0x4340_0000 then 1.0 + x else x else 0.0
            val hu: Int = if k != 0 then __HI_CHECKED(u) else huInit
            val k2: Int = if k != 0 then (hu >> 20) - 1023 else k
            val c: Double = if k != 0 then if hx < 0x4340_0000 then (if k2 > 0 then 1.0 - (u - x) else x - (u - 1.0)) / u else 0 else 0.0
            val hu2: Int = if k != 0 then hu & 0x000f_ffff else hu
            val k3 = if (k != 0) && !(hu2 < 0x6_a09e) then k2 + 1 else k2
            val u2 = if k != 0 then if hu2 < 0x6_a09e then __HI_CHECKED(u, hu2 | 0x3ff0_0000) else __HI_CHECKED(u, hu2 | 0x3fe0_0000) else u
            val f2 = if k != 0 then u2 - 1.0 else f
            val hu3 = if k != 0 then if hu2 < 0x6_a09e then hu2 else (0x0010_0000 - hu2) >> 2 else hu2


            val hfsq = 0.5 * f2 * f2
            if hu3 == 0 then
              /* |f| < 2**-20 */
              if f2 == 0.0 then
                if k3 == 0 then 0.0
                else k3 * ln2_hi + (c + k3 * ln2_lo)
              else
                val R = hfsq * (1.0 - 0.66666666666666666 * f2)
                if k3 == 0 then f2 - R
                else k3 * ln2_hi - ((R - (k3 * ln2_lo + c)) - f2)
            else
              val s = f2 / (2.0 + f2)
              val z = s * s
              val R = z * (Lp1 + z * (Lp2 + z * (Lp3 + z * (Lp4 + z * (Lp5 + z * (Lp6 + z * Lp7))))))

              if k3 == 0 then f2 - (hfsq - s * (hfsq + R))
              else k3 * ln2_hi - ((hfsq - (s * (hfsq + R) + (k3 * ln2_lo + c))) - f2)
    }.ensuring( res =>
      (res.isNaN == (x.isNaN || x < -1))
        && ((x.isFinite && x == -1.0) == res.isNegInfinity)
        && ((x.isZero && x.isPositive) == (res.isZero && res.isPositive))
        && ((x.isZero && x.isNegative) == (res.isZero && res.isNegative))
        && (x.isPositive == res.isPositive)
        && ((x.isNegative &&  x >= -1) == res.isNegative)
        && (x.isPosInfinity == res.isPosInfinity)
    )
  }
}
