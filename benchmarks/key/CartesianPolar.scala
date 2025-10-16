import stainless.annotation.*
import stainless.math._
import stainless.lang._

/** 2D Cartesian coordinates
  */
case class Cartesian(x: Double, y: Double) {

  /** Distance between two points
    */
  def distanceTo(that: Cartesian): Double = {
    require(x.isFinite)
    require(y.isFinite)
    require(that.x.isFinite)
    require(that.y.isFinite)

    val dx = x - that.x
    val dy = y - that.y

    sqrt(dx * dx + dy * dy)
  }.ensuring(!_.isNaN)

  /** Conversion to polar coordinates
    */
  def toPolar(): Polar = {
    require(x.isFinite)
    require(y.isFinite)
    require(x != 0.0 || y != 0.0)

    val radius = sqrt(x * x + y * y)
    val radiant = atan(y / x)
    val theta = radiant * (180.0 / Math.PI) // angle in degree

    Polar(radius, theta)
  }.ensuring(res => !res.radius.isNaN && !res.theta.isNaN)
}

/** 2D Polar coordinates
  */
case class Polar(radius: Double, theta: Double) {

  /** Conversion to Cartesian coordinates
    */
  def toCartesian(): Cartesian = {
    require(radius.isFinite)
    require(0 < theta && theta < 270.0)

    val radiant = theta * (Math.PI / 180.0)
    val x = radius * cos(radiant)
    val y = radius * sin(radiant)
    Cartesian(x, y)
  }.ensuring(res => !res.x.isNaN && !res.y.isNaN)

}
