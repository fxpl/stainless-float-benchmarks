package annot1_rectangularborder

import stainless.lang.*

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/utils/Point.scala#L4

/** Cartesian point */
trait Point {

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
  require((maxHorizontalPoint - minHorizontalPoint).isFinite && (maxVerticalPoint - minVerticalPoint).isFinite)
  require(minHorizontalPoint <= currentPosition.x && currentPosition.x <= maxHorizontalPoint)
  require(minVerticalPoint <= currentPosition.y && currentPosition.y <= maxVerticalPoint)

  private val cellElasticity: Double = 1.0
  private val borderElasticity: Double = 1.0

  // TO SPECIFY: 211
  private def getLowerBoundary(radius: Double, centerCoordinate: Double, borderLength: Double): Double = {
    // We get a weird specification since this function is the one used to compute `minVerticalPoint` and `minHorizontalPoint` in the original.
    // Here, we already have access to the result via `this`; we basically just state that the result is equal to itself.
    require(!radius.isNaN)
    require(!(centerCoordinate - borderLength / 2 + radius).isNaN)
    require(minHorizontalPoint == centerCoordinate - borderLength / 2 + radius || minVerticalPoint == centerCoordinate - borderLength / 2 + radius)
    centerCoordinate - borderLength / 2 + radius
  }.ensuring(res => res == minHorizontalPoint || res == minVerticalPoint)

  // TO SPECIFY: 212
  private def getUpperBoundary(radius: Double, centerCoordinate: Double, borderLength: Double): Double = {
    require(!radius.isNaN)
    require(!(centerCoordinate + borderLength / 2 - radius).isNaN)
    require(maxHorizontalPoint == centerCoordinate + borderLength / 2 - radius || maxVerticalPoint == centerCoordinate + borderLength / 2 - radius)
    centerCoordinate + borderLength / 2 - radius
  }.ensuring(res => res == maxHorizontalPoint || res == maxVerticalPoint)

  // TO SPECIFY: 213
  protected def computeNewRadius(): Double = {
    require(!entityRadius.isNaN && 0d <= entityRadius)
    val resX = getPortionOutsideBorder(currentPosition.x, minHorizontalPoint, maxHorizontalPoint)
    val resY = getPortionOutsideBorder(currentPosition.y, minVerticalPoint, maxVerticalPoint)
    entityRadius - resX - resY
  }.ensuring(_ == entityRadius) // looks wierd, but is true due to `getPotionOutsideBorder()`

  // TO SPECIFY: 214
  private def getPortionOutsideBorder(position: Double, minReachablePosition: Double, maxReachablePosition: Double): Double = {
    require(!position.isNaN && !minReachablePosition.isNaN && !maxReachablePosition.isNaN)
    position match {
      case p if p < minReachablePosition => minReachablePosition - p
      case p if p > maxReachablePosition => p - maxReachablePosition
      case _ => 0 // the entity is inside the map edge => radius portion outside the map is 0
    }
  }.ensuring(res => 0d <= res) // IDK what this function is supposed to do, it does not even use the radius.

}