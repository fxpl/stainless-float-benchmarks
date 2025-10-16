package annot0_rectangularborder

import stainless.lang.*

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/utils/Point.scala#L4

/** Cartesian point */
trait Point {

  require(x.isFinite && y.isFinite)

  /** Getter. Return the x coordinate of the point.
    *
    * @return x coordinate
    */
  val x: Double

  /** Getter. Return the y coordinate of the point.
    *
    * @return y coordinate
    */
  val y: Double
}

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/systems/borderconditions/RectangularBorder.scala

/** Collision implementation for a playing field with rectangular shape */
case class RectangularBorder(
    currentPosition: Point,
    maxHorizontalPoint: Double,
    minHorizontalPoint: Double,
    maxVerticalPoint: Double,
    minVerticalPoint: Double,
    entityRadius: Double
) {

  require(maxHorizontalPoint.isFinite)
  require(minHorizontalPoint.isFinite)
  require(maxVerticalPoint.isFinite)
  require(minVerticalPoint.isFinite)
  require(minHorizontalPoint < maxHorizontalPoint)
  require(minVerticalPoint < maxVerticalPoint)
  require(entityRadius.isFinite && entityRadius >= 0)
  require(currentPosition.x + entityRadius <= maxHorizontalPoint)
  require(currentPosition.x - entityRadius >= minHorizontalPoint)
  require(currentPosition.y + entityRadius <= maxVerticalPoint)
  require(currentPosition.y - entityRadius >= minVerticalPoint)

  private val cellElasticity: Double = 1.0
  private val borderElasticity: Double = 1.0

  // TO SPECIFY: 211
  private def getLowerBoundary(radius: Double, centerCoordinate: Double, borderLength: Double): Double = {
    require(radius.isFinite && radius >= 0)
    require(borderLength.isFinite && borderLength >= 0)
    require(centerCoordinate.isFinite)
    centerCoordinate - borderLength / 2 + radius
  }.ensuring(res => !res.isNaN)

  // TO SPECIFY: 212
  private def getUpperBoundary(radius: Double, centerCoordinate: Double, borderLength: Double): Double = {
    require(radius.isFinite && radius >= 0)
    require(borderLength.isFinite && borderLength >= 0)
    require(centerCoordinate.isFinite)
    centerCoordinate + borderLength / 2 - radius
  }.ensuring(res => !res.isNaN)

  // TO SPECIFY: 213
  protected def computeNewRadius(): Double = {
    val resX = getPortionOutsideBorder(currentPosition.x, minHorizontalPoint, maxHorizontalPoint)
    val resY = getPortionOutsideBorder(currentPosition.y, minVerticalPoint, maxVerticalPoint)
    entityRadius - resX - resY
  }.ensuring(res => res >= 0)

  // TO SPECIFY: 214
  private def getPortionOutsideBorder(position: Double, minReachablePosition: Double, maxReachablePosition: Double): Double = {
    require(minReachablePosition.isFinite && maxReachablePosition.isFinite)
    require(minReachablePosition < maxReachablePosition)
    require(!position.isNaN)
    position match {
      case p if p < minReachablePosition => minReachablePosition - p
      case p if p > maxReachablePosition => p - maxReachablePosition
      case _ => 0 // the entity is inside the map edge => radius portion outside the map is 0
    }
  }.ensuring(res => res >= 0)

}