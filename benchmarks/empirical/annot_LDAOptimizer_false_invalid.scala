package annot

import stainless.lang.* 
import stainless.math

// https://github.com/lmd1993/spark-branch-1.6/blob/b1a9c32c82e5154b3296348edefca8a3b782371d/mllib/src/main/scala/org/apache/spark/mllib/clustering/LDAOptimizer.scala
class OnlineLDAOptimizer(var tau0: Double = 1024, var kappa: Double = 0.51, var iteration: Int = 0) {
  require(iteration >= 0)
  require(kappa.isFinite && kappa > 0)
  require(tau0.isFinite && tau0 > 0)

  /**
   * A (positive) learning parameter that downweights early iterations. Larger values make early
   * iterations count less.
   */
  def getTau0: Double = this.tau0

  /**
   * Learning rate: exponential decay rate
   */
  def getKappa: Double = this.kappa

  // TO SPECIFY: 394
  /** Calculate learning rate rho for the current [[iteration]]. */
  private def rho(): Double = {
    math.pow(getTau0 + this.iteration, -getKappa)
  }.ensuring(res => res >= 0)

}
