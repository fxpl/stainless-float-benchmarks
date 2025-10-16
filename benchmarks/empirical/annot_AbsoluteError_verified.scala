package annot

//https://github.com/phenixmzy/spark-2.1.0/blob/bb535a80492de7ac696107559bf314b800dd9fc6/mllib/src/main/scala/org/apache/spark/mllib/tree/loss/AbsoluteError.scala

import stainless.math
import stainless.lang.*

/**
 * :: DeveloperApi ::
 * Class for absolute error loss calculation (for regression).
 *
 * The absolute (L1) error is defined as:
 *  |y - F(x)|
 * where y is the label and F(x) is the model prediction for features x.
 */
object AbsoluteError {

  // TO SPECIFY: 165
  def computeError(prediction: Double, label: Double): Double = {
    require(prediction.isFinite && label.isFinite)
    val err = label - prediction
    math.abs(err)
  }.ensuring(res => res >= 0)
}