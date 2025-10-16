package annot_verified

import stainless.lang.*
import stainless.math
import stainless.annotation.*
import scala.compiletime.ops.boolean

// https://github.com/gvr/metius/blob/87a2a94a67c1f20e7491da00846bb4cf6d61d24e/src/it/scala/metius/real/PolynomialPerformance.scala

object PolynomialPerformance {
  // associativity of multiplication!!!

  // TO SPECIFY: 397
  private def random(limit: Double): Double = {
    require(limit.isFinite)
    if (random_bool()) -limit * math_random() else limit * math_random()
  }.ensuring(res => -math.abs(limit) <= res && res <= math.abs(limit))

  // TO SPECIFY: 398
  private def randomParameter(): Double = {
    random(1000.0)
  }.ensuring(res => -1000 < res && res <= 1000)

  // TO SPECIFY: 399
  private def randomVariable(): Double = {
    random(10.0)
  }.ensuring(res => -10 < res && res <= 10)
}

@extern
def math_random(): Double = {
  ???.asInstanceOf[Double]
}.ensuring(res => res.isPositive && 0.0 <= res && res < 1.0)

@extern
def random_bool(): Boolean = {
  ???.asInstanceOf[Boolean]
}