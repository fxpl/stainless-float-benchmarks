package annot1

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
    math.sqrt(x * x + y * y).toFloat
  }.ensuring(res => 0d <= res && res.isInfinity == (x.isInfinity || y.isInfinity))

  // TO SPECIFY: 418
  def dotProduct(vec: Vector2): Float = {
    x * vec.x + y * vec.y
  }.ensuring(!_.isNaN)

  
  def -(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(x: Float, y: Float): Vector2 = {
    require(!x.isNaN && !y.isNaN)
    Vector2(this.x - x, this.y - y)
  }

  // TO SPECIFY: 419
  def distanceTo(vec: Vector2): Float = {
    (this - vec).magnitude
  }.ensuring(0d <= _)




}