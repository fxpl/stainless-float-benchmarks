package annot

import stainless.math.{Pi => π, _}
import stainless.lang.*

// https://github.com/gvr/metius/blob/87a2a94a67c1f20e7491da00846bb4cf6d61d24e/src/main/scala/metius/sphere/Spherical.scala

/**
  * Coordinate system (φ, λ) with
  * φ latitude
  * λ longitude
  */
object Spherical {

  // TO SPECIFY: 241
  private def square(x: Double): Double = {
    require(!x.isNaN)
    x * x
  }.ensuring(0d <= _)

  // for angles 0 ≤ α ≤ 2π
  // TO SPECIFY: 242
  def oppositeAngle(α: Double): Double = {
    require(!α.isNaN && 0 <= α && α <= 2 * π) // I strongly dislike having to specify that alpha is not NaN here, it's completely redundant
    if (α < π) α + π
    else α - π
  }.ensuring(res => 0 <= res && res <= 2 * π)


  // TO SPECIFY: 243
  def cosineLaw(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π) // again, why do I have to insert redundant NaN checks
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    acos(sin(φ1) * sin(φ2) + cos(φ1) * cos(φ1) * cos(λ2 - λ1))
  }.ensuring(res => 0d <= res && res <= π)

  // TO SPECIFY: 244
  def linearApproximation(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π)
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    hypot(cos(φ1) * (λ2 - λ1), φ2 - φ1)
  }.ensuring(res => 0d <= res && res <= 8.885765876316732)
  // I think 8.885765876316732 \approx sqrt((2π)^2 + (2π)^2) is the value computed when (φ1, λ1, φ2, λ2) = (-π, 2π, π, 0).
  // But, it would probably make more sense to only allow inputs where the approximation works relatively well.
  // I'm not sure what input ranges would achieve this, however.

  // TO SPECIFY: 245
  def haversine(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π)
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    val Δλ = λ2 - λ1
    val Δφ = φ2 - φ1
    // the square of half the cord length between the points
    val a = square(sin(0.5 * Δφ)) + cos(φ1) * cos(φ2) * square(sin(0.5 * Δλ))
    2.0 * atan2(sqrt(a), sqrt(1.0 - a))
  }.ensuring(res => 0d <= res && res <= 1)

  // TO SPECIFY: 246
  def vincenty(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π)
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    val sinφ1 = sin(φ1)
    val sinφ2 = sin(φ2)
    val cosφ1 = cos(φ1)
    val cosφ2 = cos(φ1)
    val Δλ = λ2 - λ1
    val cosΔλ = cos(Δλ)
    val sinΔλ = sin(Δλ)
    val y = hypot(cosφ2 * sinΔλ, cosφ1 * sinφ2 - sinφ1 * cosφ2 * cosΔλ)
    val x = sinφ1 * sinφ2 + cosφ1 * cosφ2 * cosΔλ
    atan2(y, x)
  }.ensuring(res => 0d <= res && res <= π)

  // TO SPECIFY: 247
  def initialBearing(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π)
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    val Δλ = λ2 - λ1
    atan2(cos(φ2) * sin(Δλ), cos(φ1) * sin(φ2) - sin(φ1) * cos(φ2) * cos(Δλ)) // what signs can be generated here?
  }.ensuring(res => -π <= res && res <= π)

  // TO SPECIFY: 248
  def finalBearing(φ1: Double, λ1: Double, φ2: Double, λ2: Double): Double = {
    require(!λ1.isNaN && 0 <= λ1 && λ1 <= 2 * π)
    require(!λ2.isNaN && 0 <= λ2 && λ2 <= 2 * π)
    require(!φ1.isNaN && -π <= φ1 && φ1 <= π)
    require(!φ2.isNaN && -π <= φ2 && φ2 <= π)
    oppositeAngle(initialBearing(λ2, φ2, λ1, φ1)) // is the order here wrong?  or, maybe the pre-conditions for `initialBearing` should be weakened a bit.
  }.ensuring(res => -π <= res && res <= π)

}