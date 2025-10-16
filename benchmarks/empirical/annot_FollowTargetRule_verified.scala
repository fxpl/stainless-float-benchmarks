package annot_follow_target_rule_verified

import stainless.lang.*
import stainless.math

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/utils/MathUtils.scala#L5

/** Utility class that offers math related useful methods */
object MathUtils {

  /** Returns the unit vector from point2 to point1
    *
    * @param point1 the unit vector's ending point
    * @param point2 the unit vector's starting point
    * @return unitVector the unit vector from point2 to point1
    */
  def unitVector(point1: Point, point2: Point): Vector = {
    point1.subtract(point2).normalized()
  }

  /** Returns the Euclidean distance in 2D space
    *
    * @param point1 first point
    * @param point2 second point
    * @return Euclidean distance
    */
  def euclideanDistance(point1: Point, point2: Point): Double = {
    val left = if point1.x.isInfinite then point1.x else point1.x - point2.x
    val right = if point1.y.isInfinite then point1.y else point1.y - point2.y
    math.sqrt(math.pow(left, 2) + math.pow(right, 2))
  }.ensuring(res => res >= 0)
}

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/utils/Constants.scala#L33

object Sentient {
    final val MaxSpeed: Double = 2
    final val MaxAcceleration: Double = 0.1
    //extra radius for safety area
    final val CoefficientDesiredSeparation: Double = 50
    final val PercentageOfLostRadiusForMagnitudeAcceleration: Double = 0.02
    final val MinRadiusForLostRadiusBehaviour: Double = 15
    final val WeightOfEscapeAccelerationFromEnemies: Double = 2
    final val WeightOfEscapeAccelerationFromBoundary: Double = WeightOfEscapeAccelerationFromEnemies * 1.5
}


// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/master/src/main/scala/it/unibo/osmos/redux/utils/Vector.scala#L8

/** Mixin representing a 2D vector.
  *
  * The methods of this mixin won't alter
  * the values of the classes that extends it.
  */
case class Vector(val x: Double, val y: Double) {
  require(!x.isNaN && !y.isNaN)


  /** Gets the magnitude (module) of the vector applying parallelogram law
    *
    * @return magnitude of this vector
    */
  def getMagnitude: Double = {
    math.sqrt(math.pow(x, 2) + math.pow(y, 2))
  }.ensuring(res => res >= 0)

  /** Gets the vector normalized
    *
    * @return a new normalized instance of this vector
    */
  def normalized(): Vector = {
    val magnitude = getMagnitude
    if magnitude.isInfinite then 
      val e1 = if x.isPositive then 1 else -1 
      val e2 = if y.isPositive then 1 else -1
      if math.abs(x) == math.abs(y) then Vector(e1 / math.sqrt(2), e2 / math.sqrt(2))
      else if math.abs(x) > math.abs(y) then Vector(e1, 0)
      else Vector(0, e2)
    else if (magnitude != 0) {
      this.divide(magnitude)
    } else {
      Vector(x, y)
    }
  }

  /** Vector subtraction. Leaves this vector unchanged.
    *
    * @param v vector to subtract
    * @return subtraction result as a new instance
    */
  def subtract(v: Vector): Vector = {
    val left = if x.isInfinite then x else x - v.x
    val right = if y.isInfinite then y else y - v.y
    Vector(left, right)
  }

  /** Vector-scalar multiplication. Leaves this vector unchanged.
    *
    * @param v scalar to multiply
    * @return multiplication mu result as a new instance
    */
  def multiply(v: Double): Vector = {
    require(v.isFinite)
    if v == 0 then Vector(0, 0) else Vector(x * v, y * v)
  }


  /** Vector-scalar division. Leaves this vector unchanged.
    *
    * @param v scalar
    * @return division result as a new instance
    */
  def divide(v: Double): Vector = {
    require(v.isFinite && v != 0)
    Vector(x / v, y / v)
  }

  /** Limits the vector's magnitude if it is greater than the given one
    *
    * @param maxMagnitude the max magnitude of the vector
    * @return a new vector
    */
  def limit(maxMagnitude: Double): Vector = {
    require(maxMagnitude.isFinite && maxMagnitude >= 0)
    if (getMagnitude > maxMagnitude) {
      getNewMagnitude(maxMagnitude)
    } else {
      Vector(x, y)
    }
  }

  /** Scales this vector magnitude (module) with the desired one
    *
    * @param desiredMagnitude desired vector magnitude
    * @return a new vector with the specified module
    */
  def getNewMagnitude(desiredMagnitude: Double): Vector = {
    require(desiredMagnitude.isFinite && desiredMagnitude >= 0)
    val scale = if desiredMagnitude.isZero then 0 else desiredMagnitude / getMagnitude
    if scale.isInfinite then
      val right = if x.isZero then 0 else if x.isPositive then desiredMagnitude else -desiredMagnitude
      val left = if y.isZero then 0 else if y.isPositive then desiredMagnitude else -desiredMagnitude
      Vector(left, right)
    else Vector(x, y).multiply(scale)
  }
}

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/utils/Point.scala#L4

/** Cartesian point */
case class Point( val x: Double, val y: Double) {

  require(!x.isNaN && !y.isNaN)

  /** Point-vector addition.
    *
    * @param v vector to add
    * @return the addition result as a new Point instance
    */
  def add(v: Vector): Point = {
    val left = if x.isInfinite  then x else x + v.x
    val right = if y.isInfinite then y else y + v.y
    Point(left, right)
  }

  /** Point-point subtraction.
    *
    * @param p point to subtract
    * @return the subtraction result as a new Vector instance
    */
  def subtract(p: Point): Vector = {
    val left = if x.isInfinite then x else x - p.x
    val right = if y.isInfinite then y else y - p.y
    Vector(left, right)
  }
}


// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/entities/properties/composed/SentientProperty.scala#L6

/** Trait representing the properties needed by an entity to be sentient */
case class SentientProperty(getDimensionComponent: Double, getPositionComponent: Point) {
    require(getDimensionComponent.isFinite && getDimensionComponent >= 0)
}

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/entities/properties/composed/SentientEnemyProperty.scala#L8

/** Trait representing the properties needed by an entity
  * to be seen as an enemy by the sentient cells
  */
case class SentientEnemyProperty(val getSpeedComponent: Vector, val getDimensionComponent: Double, val getPositionComponent: Point) {
    require(getDimensionComponent.isFinite && getDimensionComponent >= 0)
}


// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/systems/sentientrules/SentientUtils.scala


/** Utils for sentient system and rules */
object SentientUtils {

  /** compute the acceleration to apply to the actual velocity and obtain the desired velocity
    *
    * @param actualVelocity  actual velocity
    * @param desiredVelocity desired velocity
    * @return the acceleration
    */
  def computeUnlimitedSteer(actualVelocity: Vector, desiredVelocity: Vector): Vector = {
    desiredVelocity.multiply(Sentient.MaxSpeed).subtract(actualVelocity)
  }

}


// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/systems/sentientrules/SentientRule.scala

/** Base rule for sentient entity */
sealed trait SentientRule {

  /** compute the acceleration to apply for this rule to the specified entity with the previous compute acceleration
    *
    * @param sentient             entity to apply the rule
    * @param previousAcceleration previous acceleration compute
    * @return acceleration to apply this rule considered the previous acceleration
    */
  def computeRule(sentient: SentientProperty, previousAcceleration: Vector): Vector
}

// https://github.com/PPS-Osmos-Redux/PPS-17-osmos-redux/blob/b0cf07b1fdde406ecd481713ce74df5272c20377/src/main/scala/it/unibo/osmos/redux/ecs/systems/sentientrules/FollowTargetRule.scala

/** Rule to compute the acceleration to follow the target enemy
  *
  * @param enemies list of enemies from which choose the target enemy
  */
trait FollowTargetRule  extends SentientRule {
  /** compute the coefficient with lost radius
    *
    * @param sentient sentient entity
    * @param enemy    sentient enemy entity
    * @return the coefficient representing the radius that can be gained form an enemy
    */
  // TO SPECIFY: 273
  private def targetCoefficientWithLostRadius(sentient: SentientProperty, enemy: SentientEnemyProperty, escapeVelocity: Vector): Double = {
    val nextPositionTarget = enemy.getPositionComponent.add(enemy.getSpeedComponent)
    val unitVectorDesiredVelocity = MathUtils.unitVector(nextPositionTarget, sentient.getPositionComponent)
    val magnitudeOfRotation = SentientUtils.computeUnlimitedSteer(escapeVelocity, unitVectorDesiredVelocity).getMagnitude
    val lostRadiusPercentage = magnitudeOfRotation * Sentient.PercentageOfLostRadiusForMagnitudeAcceleration

    
    enemy.getDimensionComponent - (sentient.getDimensionComponent * lostRadiusPercentage)
  }.ensuring(res => res >= 0)

  /** compute the coefficient without lost radius
    *
    * @param sentient sentient entity
    * @param enemy    sentient enemy entity
    * @return a coefficient directly proportional to the enemy's radius and
    *         inversely proportional to the distance between the entities
    */
  // TO SPECIFY: 274
  private def targetCoefficientWithoutLostRadius(sentient: SentientProperty, enemy: SentientEnemyProperty): Double = {
    enemy.getDimensionComponent / MathUtils.euclideanDistance(sentient.getPositionComponent, enemy.getPositionComponent)
  }.ensuring(res => res >= 0)
}