package annot1

import stainless.math
import stainless.lang.*

// https://github.com/litan/kojo-lib/blob/4083e51bba1ca6b1fb09c5f4711fd277f06910e2/src/main/scala/net/kogics/kojo/core/shapes.scala#L39

class Point(val x: Double, val y: Double) {
  require(!x.isNaN)
  require(!y.isNaN)
}

// https://github.com/litan/kojo-lib/blob/4083e51bba1ca6b1fb09c5f4711fd277f06910e2/src/main/scala/net/kogics/kojo/kmath/Kmath.scala

object KMath {
  // TO SPECIFY: 349
  def distance(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    require(!(x1 - x2).isNaN)
    require(!(y1 - y2).isNaN)
    math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
  }.ensuring(0d <= _)

  // TO SPECIFY: 350
  def distance(p1: Point, p2: Point): Double = {
    require(!(p1.x - p2.x).isNaN)
    require(!(p1.y - p2.y).isNaN)
    distance(p1.x, p1.y, p2.x, p2.y)
  }.ensuring(0d <= _)

  // TO SPECIFY: 351
  def angle(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    require(!(x1 - x2).isNaN)
    require(!(y1 - y2).isNaN)
    math.atan2(y2 - y1, x2 - x1).toDegrees
  }.ensuring(res => -180d <= res && res <= 180d)

  // TO SPECIFY: 352
  def angle(p1: Point, p2: Point): Double = {
    require(!(p1.x - p2.x).isNaN)
    require(!(p1.y - p2.y).isNaN)
    angle(p1.x, p1.y, p2.x, p2.y)
  }.ensuring(res => -180d <= res && res <= 180d)
}