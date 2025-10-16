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

  object Log {
    private val ln2_hi: Double = java.lang.Double.longBitsToDouble(0x3fe62e42fee00000L)// 6.93147180369123816490e-01
    private val ln2_lo = java.lang.Double.longBitsToDouble(0x3dea39ef35793c76L) // 1.90821492927058770002e-10
    private val Lg1 = java.lang.Double.longBitsToDouble(0x3fe5555555555593L) // 6.666666666666735130e-01
    private val Lg2 = java.lang.Double.longBitsToDouble(0x3fd999999997fa04L) // 3.999999999940941908e-01
    private val Lg3 = java.lang.Double.longBitsToDouble(0x3fd2492494229359L) // 2.857142874366239149e-01
    private val Lg4 = java.lang.Double.longBitsToDouble(0x3fcc71c51d8e78afL)  // 2.222219843214978396e-01
    private val Lg5 = java.lang.Double.longBitsToDouble(0x3fc7466496cb03deL) // 1.818357216161805012e-01
    private val Lg6 = java.lang.Double.longBitsToDouble(0x3fc39a09d078c69fL) // 1.531383769920937332e-01
    private val Lg7 = java.lang.Double.longBitsToDouble(0x3fc2f112df3e5244L) // 1.479819860511658591e-01

    @opaque
    def computeLog(xInit: Double): Double = {

      if xInit.isNaN then Double.NaN
      else

        var hxInit = __HI_CHECKED(xInit) // high word of x
        val lx = __LO_CHECKED(xInit) // low  word of x, unsigned

        if hxInit < 0x0010_0000 && (((hxInit & EXP_SIGNIF_BITS) | lx) == 0) then -TWO54 / 0.0
        else if hxInit < 0 then (xInit - xInit) / 0.0
        else
          val xInit2 = if hxInit < 0x0010_0000 then xInit * TWO54 else xInit
          val hxInit2 = if hxInit < 0x0010_0000 then __HI_CHECKED(xInit2) else hxInit

          if hxInit2 >= EXP_BITS then  xInit2 + xInit2
          else
            val kInit: Int = (if hxInit < 0x0010_0000 then -54 else 0) + ((hxInit2 >> 20) - 1023)
            val hx: Int = hxInit2 & 0x000f_ffff
            val i: Int = (hx + 0x9_5f64) & 0x10_0000
            val x: Double = __HI_CHECKED(xInit2, hx | (i ^ 0x3ff0_0000)) // normalize x or x/2

            val k: Int = kInit + (i >> 20)
            val f = x - 1.0

            if (0x000f_ffff & (2 + hx)) < 3 then // |f| < 2**-20
              if f == 0.0 then
                if k == 0 then 0.0
                else
                  val dk = k.toDouble
                  dk * ln2_hi + dk * ln2_lo

              else
                val R = f * f * (0.5 - 0.33333333333333333 * f)
                if k == 0 then f - R
                else
                  val dk = k.toDouble
                  dk * ln2_hi - ((R - dk * ln2_lo) - f)
            else
              val s = f / (2.0 + f)
              val dk = k.toDouble
              val z = s * s
              val j = 0x6b851 - hx
              val i2 = (hx - 0x6_147a) | j
              val w = z * z
              val t1 = w * (Lg2 + w * (Lg4 + w * Lg6))
              val t2 = z * (Lg1 + w * (Lg3 + w * (Lg5 + w * Lg7)))
              val R = t2 + t1

              if i2 > 0 then
                val hfsq = 0.5 * f * f
                if k == 0 then f - (hfsq - s * (hfsq + R))
                else dk * ln2_hi - ((hfsq - (s * (hfsq + R) + dk * ln2_lo)) - f)
              else if k == 0 then f - s * (f - R)
              else dk * ln2_hi - ((s * (f - R) - dk * ln2_lo) - f)
    }.ensuring( res =>
      (res.isNaN == (xInit.isNaN || xInit < 0))
      && (xInit.isZero == res.isNegInfinity)
      && ((xInit.isFinite && xInit == 1.0) == (res.isZero && res.isPositive))
      && ((!xInit.isNaN && xInit >= 1.0) == res.isPositive)
      && (res.isNegative == (xInit.isFinite && 0.0 <= xInit && xInit < 1.0))
      && ((!xInit.isNaN && xInit >= 0) ==> res <= xInit - 1)
      && (xInit.isPosInfinity == res.isPosInfinity)
    )
  }
}
