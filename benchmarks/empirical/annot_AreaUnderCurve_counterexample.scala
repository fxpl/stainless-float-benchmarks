package annot

import stainless.lang.* 
import stainless.collection.*

// https://github.com/lmd1993/spark-branch-1.6/blob/b1a9c32c82e5154b3296348edefca8a3b782371d/mllib/src/main/scala/org/apache/spark/mllib/evaluation/AreaUnderCurve.scala

/**
 * Computes the area under the curve (AUC) using the trapezoidal rule.
 */
object AreaUnderCurve {

  /**
   * Uses the trapezoidal rule to compute the area under the line connecting the two input points.
   * @param points two 2D points stored in Seq
   */
  // TO SPECIFY: 449
  private def trapezoid(points: Array[(Double, Double)]): Double = {
    require(points.length == 2)
    require(points(0)._1.isFinite && points(1)._1.isFinite)
    require(points(0)._2.isFinite && points(1)._2.isFinite)
    require(points(0)._1 < points(1)._1) 
    val x = points(0)
    val y = points(1)
    (y._1 - x._1) * (y._2 + x._2) / 2.0
  }.ensuring(res => !res.isNaN)
}