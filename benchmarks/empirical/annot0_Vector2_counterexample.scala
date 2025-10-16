package annot0

import stainless.lang.*
import stainless.math

// https://github.com/jpbetz/subspace/blob/57fc0a1c66b70fc69b233bab46be6bbbf00a1462/subspace/src/main/scala/com/github/jpbetz/subspace/Vector2.scala

case class Vector2(x: Float, y: Float) {

  require(x.isFinite && y.isFinite)

  def apply(index: Int): Float = {
    require(index == 0 || index == 1)
    index match {
      case 0 => x
      case 1 => y
    }
  }

  // TO SPECIFY: 417
  def magnitude: Float = {
    math.sqrt(x * x + y * y).toFloat
  }.ensuring(res => res >= 0)

  // TO SPECIFY: 418
  def dotProduct(vec: Vector2): Float = {
    x * vec.x + y * vec.y
  }.ensuring(res => !res.isNaN)

  
  def -(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(vec: Vector2): Vector2 = subtract(vec.x, vec.y)
  def subtract(x: Float, y: Float): Vector2 = {
    require(x.isFinite && y.isFinite)
    Vector2(this.x - x, this.y - y)
  }

  // TO SPECIFY: 419
  def distanceTo(vec: Vector2): Float = {
    (this - vec).magnitude
  }.ensuring(res => res >= 0)




}