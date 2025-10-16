package annot1_verified

import stainless.lang.*
import stainless.math

// https://github.com/jpbetz/subspace/blob/57fc0a1c66b70fc69b233bab46be6bbbf00a1462/subspace/src/main/scala/com/github/jpbetz/subspace/Vector2.scala

case class Vector2(x: Float, y: Float) {
  require(!x.isNaN)
  require(!y.isNaN)

  def apply(index: Int): Float = {
    require(index == 0 || index == 1) // already here
    index match {
      case 0 => x
      case 1 => y
    }
  }

  // TO SPECIFY: 417
  def magnitude: Float = {
    math.hypot(x, y).toFloat
  }.ensuring(res => 0d <= res && res.isInfinity == (x.isInfinity || y.isInfinity))

  // TO SPECIFY: 418
  def dotProduct(vec: Vector2): Float = {
    if x.isInfinite && vec.x.isPositive then x
    else if x.isInfinite && vec.x.isNegative then -x
    else if vec.x.isInfinite && x.isPositive then vec.x
    else if vec.x.isInfinite && x.isNegative then -vec.x
    else if y.isInfinite && vec.y.isPositive then y
    else if y.isInfinite && vec.y.isNegative then -y
    else if vec.y.isInfinite && y.isPositive then vec.y
    else if vec.y.isInfinite && y.isNegative then -vec.y
    else
      val left = x * vec.x
      val right = y * vec.y
      if left.isPosInfinity then Float.PositiveInfinity
      else if left.isNegInfinity then Float.NegativeInfinity
      else
        left + right
  }.ensuring(!_.isNaN)

  
  def -(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(x: Float, y: Float): Vector2 = {
    require(!x.isNaN && !y.isNaN)
    val first = if this.x.isInfinite then this.x else this.x - x
    val second = if this.y.isInfinite then this.y else this.y - y
    Vector2(first, second)
  }

  // TO SPECIFY: 419
  def distanceTo(vec: Vector2): Float = {
    (this - vec).magnitude
  }.ensuring(0d <= _)




}