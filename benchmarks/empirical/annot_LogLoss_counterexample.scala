package annot

import stainless.math
import stainless.lang.*

// https://github.com/MrLight0809/spark/blob/0863ff039b1f579e40e6cd5f6dc48af582d4c1c4/mllib/src/main/scala/org/apache/spark/mllib/util/MLUtils.scala

/**
 * Helper methods to load, save and pre-process data used in MLLib.
 */
object MLUtils {

  /**
   * When `x` is positive and large, computing `math.log(1 + math.exp(x))` will lead to arithmetic
   * overflow. This will happen when `x > 709.78` which is not a very large number.
   * It can be addressed by rewriting the formula into `x + math.log1p(math.exp(-x))` when `x > 0`.
   * @param x a floating-point value as input.
   * @return the result of `math.log(1 + math.exp(x))`.
   */
  def log1pExp(x: Double): Double = {
    require(!x.isNaN)
    if (x > 0) {
      x + math.log1p(math.exp(-x))
    } else {
      math.log1p(math.exp(x))
    }
  }.ensuring(res => res >= 0)
}

// https://github.com/MrLight0809/spark/blob/0863ff039b1f579e40e6cd5f6dc48af582d4c1c4/mllib/src/main/scala/org/apache/spark/mllib/tree/loss/LogLoss.scala
/**
 * :: DeveloperApi ::
 * Class for log loss calculation (for classification).
 * This uses twice the binomial negative log likelihood, called "deviance" in Friedman (1999).
 *
 * The log loss is defined as:
 *   2 log(1 + exp(-2 y F(x)))
 * where y is a label in {-1, 1} and F(x) is the model prediction for features x.
 */
trait LogLoss  {

  /**
   * Method to calculate the loss gradients for the gradient boosting calculation for binary
   * classification
   * The gradient with respect to F(x) is: - 4 y / (1 + exp(2 y F(x)))
   * @param prediction Predicted label.
   * @param label True label.
   * @return Loss gradient
   */
  // Associativity matters!!!
  // TO SPECIFY: 131
  def gradient(prediction: Double, label: Double): Double = {
    require(prediction.isFinite && label.isFinite)
    -4.0 * label / (1.0 + math.exp(2.0 * label * prediction))
  }.ensuring(res => !res.isNaN) 

  // Associativity matters!!!
  // TO SPECIFY: 132
  def computeError(prediction: Double, label: Double): Double = {
    require(prediction.isFinite && label.isFinite)
    val margin = 2.0 * label * prediction
    // The following is equivalent to 2.0 * log(1 + exp(-margin)) but more numerically stable.
    2.0 * MLUtils.log1pExp(-margin)
  }.ensuring(res => res >= 0)

  /**
   * Returns the estimated probability of a label of 1.0.
   */
  // TO SPECIFY: 133
  def computeProbability(margin: Double): Double = {
    require(!margin.isNaN)
    1.0 / (1.0 + math.exp(-2.0 * margin))
  }.ensuring(res => 0 <= res && res <= 1.0)
}