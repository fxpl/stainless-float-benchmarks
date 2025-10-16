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

  object Pow {
    @opaque
    def computePow(x: Double, y: Double): Double = {
      // y == zero: x**0 = 1
      if y.isZero then 1.0
      else if x.isNaN || y.isNaN then x + y
      else
        val y_abs = stainless.math.abs(y)
        val x_abs = stainless.math.abs(x)
        // Special values of y
        if y == 2.0 then x * x
        else if y == 0.5 && x >= -Double.MaxValue then stainless.math.sqrt(x + 0.0) // Add 0.0 to properly handle x == -0.0// Handle x == -infinity later
        else if y_abs == 1.0 then // y is  +/-1
          if y == 1.0 then x else 1.0 / x
        else if y_abs == Double.PositiveInfinity then // y is +/-infinity
          if x_abs == 1.0 then y - y // inf**+/-1 is NaN
          else if x_abs > 1.0 then // (|x| > 1)**+/-inf = inf, 0
            if y >= 0 then y else 0.0
          else if y < 0 then -y else 0.0 // (|x| < 1)**-/+inf = inf, 0
        else
          val hx = __HI_CHECKED(x)
          val ix = hx & EXP_SIGNIF_BITS
          val y_is_int =
            if hx < 0 then
              if y_abs >= java.lang.Double.longBitsToDouble(0x4340000000000000L) then 2
              else if y_abs >= 1.0 then // |y| >= 1.0
                  val y_abs_as_long = y_abs.toLong
                  if y_abs_as_long.toDouble == y_abs then
                    2 - (y_abs_as_long & 0x1L).toInt
                  else 0
              else 0
            else 0

          // Special value of x
          if x_abs == 0.0 || x_abs == Double.PositiveInfinity || x_abs == 1.0 then
            val z1 = if y < 0.0 then 1.0 / x_abs else x_abs
            if hx < 0 then
              if ((ix - 0x3ff00000) | y_is_int) == 0 then (z1 - z1) / (z1 - z1) // (-1)**non-int is NaN
              else if y_is_int == 1 then -1.0 * z1 // (x < 0)**odd = -(|x|**odd)
              else z1
            else z1
          else
            val n_init = (hx >> 31) + 1
            // (x < 0)**(non-int) is NaN
            if (n_init | y_is_int) == 0 then (x - x) / (x - x)
            else
              val s = if (n_init | (y_is_int - 1)) == 0 then -1.0 else 1.0 // (-ve)**(odd int)
              val y_large_cond: Boolean = y_abs > java.lang.Double.longBitsToDouble(0x41e00000ffffffffL)

              if y_large_cond && x_abs < java.lang.Double.longBitsToDouble(0x3fefffff00000000L) then
                if y < 0.0 then s * Double.PositiveInfinity else s * 0.0 // |x| < ~0.9999995231628418
              else if y_large_cond && x_abs > java.lang.Double.longBitsToDouble(0x3ff00000ffffffffL) then
                if y > 0.0 then s * Double.PositiveInfinity else s * 0.0
              else
                val INV_LN2 = java.lang.Double.longBitsToDouble(0x3ff71547652b82feL) //  1.44269504088896338700e+00 = 1/ln2
                val INV_LN2_H = java.lang.Double.longBitsToDouble(0x3ff7154760000000L) //  1.44269502162933349609e+00 = 24 bits of 1/ln2
                val INV_LN2_L = java.lang.Double.longBitsToDouble(0x3e54ae0bf85ddf44L) //  1.92596299112661746887e-08 = 1/ln2 tail

                val CP = java.lang.Double.longBitsToDouble(0x3feec709dc3a03fdL) //  9.61796693925975554329e-01 = 2/(3ln2)
                val CP_H = java.lang.Double.longBitsToDouble(0x3feec709e0000000L) //  9.61796700954437255859e-01 = (float)cp
                val CP_L = java.lang.Double.longBitsToDouble(0xbe3e2fe0145b01f5L) // -7.02846165095275826516e-09 = tail of CP_H

                val x_abs2: Double =
                  if y_large_cond then x_abs
                  else if ix < 0x00100000 then x_abs * java.lang.Double.longBitsToDouble(0x4340000000000000L) else x_abs
                // assert(x_abs2.isFinite && x_abs2.isPositive)

                val ix2: Int =
                  if y_large_cond then ix
                  else if ix < 0x00100000 then __HI(x_abs2) else ix

                val j: Int = if y_large_cond then 0 else ix2 & 0x000fffff

                // assert(0 <= j && j <= 0x000fffff)

                val n: Int =
                  if y_large_cond then n_init
                  else
                    val n2 = if ix < 0x00100000 then -53 else 0
                    val n3 = n2 + (ix2 >> 20) - 0x3ff
                    if !(j <= 0x3988E) && !(j < 0xBB67A) then n3 + 1 else n3
                val ix3: Int =
                  if y_large_cond then ix2
                  else j | 0x3ff00000
                val k: Int =
                  if y_large_cond then 0
                  else (if j <= 0x3988E then 0 else (if j < 0xBB67A then 1 else 0))
                val ix4: Int =
                  if y_large_cond then ix3
                  else if j > 0x3988E && j >= 0xBB67A then ix3 - 0x00100000 else ix3
                val x_abs3: Double =
                  if y_large_cond then x_abs2
                  else __HI(x_abs2, ix4)

                // assert(x_abs3.isFinite)

                val t: Double =
                  if y_large_cond then x_abs3 - 1.0
                  else n.toDouble

                // assert(t.isFinite)

                val w =
                  if y_large_cond then (t * t) * (0.5 - t * (0.3333333333333333333333 - t * 0.25))
                  else 0.0

                // assert(!w.isNaN)

                // Compute ss = s_h + s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5)
                val BP = if k == 0 then 1.0 else 1.5
                val DP_H = if k == 0 then 0.0 else java.lang.Double.longBitsToDouble(0x3fe2b80340000000L) // 5.84962487220764160156e-01
                val DP_L = if k == 0 then 0.0 else java.lang.Double.longBitsToDouble(0x3e4cfdeb43cfd006L) // 1.35003920212974897128e-08

                // Poly coefs for (3/2)*(log(x)-2s-2/3*s**3
                val L1 = java.lang.Double.longBitsToDouble(0x3fe3333333333303L) //  5.99999999999994648725e-01
                val L2 = java.lang.Double.longBitsToDouble(0x3fdb6db6db6fabffL) //  4.28571428578550184252e-01
                val L3 = java.lang.Double.longBitsToDouble(0x3fd55555518f264dL) //  3.33333329818377432918e-01
                val L4 = java.lang.Double.longBitsToDouble(0x3fd17460a91d4101L) //  2.72728123808534006489e-01
                val L5 = java.lang.Double.longBitsToDouble(0x3fcd864a93c9db65L) //  2.30660745775561754067e-01
                val L6 = java.lang.Double.longBitsToDouble(0x3fca7e284a454eefL) //  2.06975017800338417784e-01

                val u: Double =
                  if y_large_cond then INV_LN2_H * t
                  else x_abs3 - BP

                // assert(u.isFinite)

                val v: Double =
                  if y_large_cond then t * INV_LN2_L - w * INV_LN2
                  else 1.0 / (x_abs3 + BP)

                // assert(v.isFinite)

                val ss = u * v

                // assert(ss.isFinite)

                val s_h = __LO(ss, 0)

                // assert(s_h.isFinite)

                val t_h = __HI(0.0, ((ix4 >> 1) | 0x20000000) + 0x00080000 + (k << 18))

                // assert(t_h.isPositive)

                val s_l = v * ((u - s_h * t_h) - s_h * (x_abs3 - (t_h - BP)))

                // assert(!s_l.isNaN)

                val s2 = ss * ss

                // assert(s2.isPositive)

                val r = (s2 * s2 * (L1 + s2 * (L2 + s2 * (L3 + s2 * (L4 + s2 * (L5 + s2 * L6)))))) + (s_l * (s_h + ss))

                // assert(!r.isNaN)

                val s3 = s_h * s_h

                // assert(s3.isFinite && s3.isPositive)

                val t_h2: Double = __LO((3.0 + s3 + r), 0)

                // assert(t_h2.isFinite)

                val t_l = r - ((t_h2 - 3.0) - s3)

                // assert(!t_l.isNaN)

                val u2 = if y_large_cond then u
                  else s_h * t_h2

                // assert(!u2.isNaN)

                val v2 = if y_large_cond then v
                  else s_l * t_h2 + t_l * ss

                // assert(!v2.isNaN)

                val p_h = __LO(u2 + v2, 0)

                // assert(!p_h.isNaN)
                val z_h = CP_H * p_h

                // assert(!z_h.isNaN)

                val z_l = CP_L * p_h + (v2 - (p_h - u2)) * CP + DP_L

                // assert(!z_l.isNaN)

                val t1 = if y_large_cond then p_h
                  else __LO((((z_h + z_l) + DP_H) + t), 0)

                // assert(!t1.isNaN)

                val t2 = if y_large_cond then v - (t1 - u)
                  else z_l - (((t1 - t) - DP_H) - z_h)

                // assert(!t2.isNaN)

                // Split up y into (y1 + y2) and compute (y1 + y2) * (t1 + t2)
                val y1: Double = __LO(y, 0)

                // assert(y1.isFinite)

                val p_l: Double = (y - y1) * t1 + y * t2
                val p_h2: Double = y1 * t1
                val z: Double = p_l + p_h2
                val j2: Int = __HI(z)
                val i: Int = __LO(z)

                // the two asserts below yield better strict-arithmetic verification times
                // assert(!p_l.isNaN)
                // assert(!(z - p_h2).isNaN)
                if j2 >= 0x40900000 && (((j2 - 0x40900000) | i) != 0 || p_l + 8.0085662595372944372e-0017 > z - p_h2) then s * Double.PositiveInfinity // Overflow
                else if (j2 & EXP_SIGNIF_BITS) >= 0x4090cc00 && (((j2 - 0xc090cc00) | i) != 0 || p_l <= z - p_h2) then s * 0.0
                else

                  // Poly coefs for (3/2)*(log(x)-2s-2/3*s**3
                  val P1 = java.lang.Double.longBitsToDouble(0x3fc555555555553eL) //  1.66666666666666019037e-01
                  val P2 = java.lang.Double.longBitsToDouble(0xbf66c16c16bebd93L) // -2.77777777770155933842e-03
                  val P3 = java.lang.Double.longBitsToDouble(0x3f11566aaf25de2cL) //  6.61375632143793436117e-05
                  val P4 = java.lang.Double.longBitsToDouble(0xbebbbd41c5d26bf1L) // -1.65339022054652515390e-06
                  val P5 = java.lang.Double.longBitsToDouble(0x3e66376972bea4d0L) //  4.13813679705723846039e-08
                  val LG2 = java.lang.Double.longBitsToDouble(0x3fe62e42fefa39efL) //  6.93147180559945286227e-01
                  val LG2_H = java.lang.Double.longBitsToDouble(0x3fe62e4300000000L) //  6.93147182464599609375e-01
                  val LG2_L = java.lang.Double.longBitsToDouble(0xbe205c610ca86c39L) // -1.90465429995776804525e-09

                  val i2 = j2 & EXP_SIGNIF_BITS
                  val k = (i2 >> 20) - 0x3ff
                  val m = if i2 > 0x3fe00000 then j2 + (0x00100000 >> (k + 1)) else 0
                  val k2 = if i2 > 0x3fe00000 then ((m & EXP_SIGNIF_BITS) >> 20) - 0x3ff else k
                  val p_h3 = if i2 > 0x3fe00000 then p_h2 - __HI(0.0, m & ~(0x000fffff >> k2)) else p_h2
                  val m2 = if i2 > 0x3fe00000 then ((m & 0x000fffff) | 0x00100000) >> (20 - k2) else m
                  val m3 = if i2 > 0x3fe00000 && j2 < 0 then -m2 else m2
                  val tt = __LO(p_l + p_h3, 0)
                  val uFinal = tt * LG2_H
                  val v3 = (p_l - (tt - p_h3)) * LG2 + tt * LG2_L
                  val z2 = uFinal + v3
                  val wFinal = v3 - (z2 - uFinal)
                  val tFinal = z2 * z2
                  val t1Final = z2 - tFinal * (P1 + tFinal * (P2 + tFinal * (P3 + tFinal * (P4 + tFinal * P5))))
                  val z3 = 1.0 - (((z2 * t1Final) / (t1Final - 2.0) - (wFinal + z2 * wFinal)) - z2)
                  // assert(!z3.isNaN)
                  val hi_z3 = __HI(z3)
                  val j3 = hi_z3 + (m3 << 20) // addition overflow check slow
                  // substituting j3 for hi_z3 + (m3 << 20) in the line below yields better strict-arithmetic performance
                  val z4 =
                    if (j3 >> 20) <= 0 then stainless.math.scalb(z3, m3) else __HI(z3, hi_z3 + (m3 << 20))
                  s * z4
    }.ensuring( res =>
      (y.isZero ==> (res == 1.0))
      && ((!x.isNaN && y.isFinite && y == 1.0) ==> (res == x))
      && (y.isNaN ==> res.isNaN)
      && ((x.isNaN && !y.isZero) ==> res.isNaN)
      && (((x.isFinite && (x < -1.0d || x > 1.0d)) && y.isPositive && y.isInfinity) ==> (res.isPositive && res.isInfinity))
      && ((x.isFinite && -1.0d < x && x < 1.0d && y.isNegative && y.isInfinity) ==> (res.isPositive && res.isInfinity))
      && (((x.isFinite && (x < -1.0d || x > 1.0d)) && y.isNegative && y.isInfinity) ==> (res.isPositive && res.isZero))
      && ((x.isFinite && -1.0d < x && x < 1.0d && y.isPositive && y.isInfinity) ==> (res.isPositive && res.isZero))
      && (((x.isFinite && (x == 1.0d || x == -1.0d)) && y.isInfinity) ==> res.isNaN)
      && ((x.isPositive && x.isZero && !y.isNaN &&y > 0) ==> (res.isPositive && res.isZero))
      && ((x.isPositive && x.isInfinity && !y.isNaN && y < 0) ==> (res.isPositive && res.isZero))
      && ((x.isPositive && x.isZero && !y.isNaN && y < 0) ==> (res.isPositive && res.isInfinity))
      && ((x.isPositive && x.isInfinity && !y.isNaN && y > 0) ==> (res.isPositive && res.isInfinity))
      && ((!x.isNaN && y.isFinite && y == 2) ==> res.isPositive)
    )
  }
}
