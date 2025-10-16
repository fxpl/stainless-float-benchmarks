package annot

// https://github.com/jiadongy/Spark.SkewAdaptive/blob/a3429c5547153dbc2320456fc0df4edecf91d762/mllib/src/main/scala/org/apache/spark/mllib/tree/impurity/Variance.scala

import stainless.lang.*

object Variance {

  /**
   * :: DeveloperApi ::
   * variance calculation
   * @param count number of instances
   * @param sum sum of labels
   * @param sumSquares summation of squares of the labels
   * @return information value, or 0 if count = 0
   */
  // TO SPECIFY: 269
  def calculate(count: Double, sum: Double, sumSquares: Double): Double = {
    require(count.isFinite && count >= 0)
    require(!sum.isNaN)
    require(!sumSquares.isNaN && sumSquares >= 0)
    require(count > 0 ==> sumSquares >= (sum * sum) / count )
    if (count == 0) {
      return 0
    }
    val squaredLoss = sumSquares - (sum * sum) / count
    squaredLoss / count
  }.ensuring(res => res >= 0)
}


/**
 * Stores statistics for one (node, feature, bin) for calculating impurity.
 * Unlike [[GiniAggregator]], this class stores its own data and is for a specific
 * (node, feature, bin).
 * @param stats  Array of sufficient statistics for a (node, feature, bin).
 */
class VarianceCalculator(stats: Array[Double]){

  require(stats.size == 3,
    s"VarianceCalculator requires sufficient statistics array stats to be of length 3," +
    s" but was given array of length ${stats.size}.")

  require(stats(0).isFinite && stats(0) >= 0)
  require(!stats(1).isNaN)
  require(!stats(2).isNaN && stats(2) >= 0)
  require(stats(0) > 0 ==> stats(2) > (stats(1) * stats(1)) / stats(0) )

  /**
   * Calculate the impurity from the stored sufficient statistics.
   */
  // TO SPECIFY: 270
  def calculate(): Double = {
    Variance.calculate(stats(0), stats(1), stats(2))
  }.ensuring(res => res >= 0)

  /**
   * Number of data points accounted for in the sufficient statistics.
   */
  def count: Long = stats(0).toLong

  /**
   * Prediction which should be made based on the sufficient statistics.
   */
  // TO SPECIFY: 271
  def predict: Double = {
    if (count == 0) {
      0
    } else {
      stats(1) / count
    }
  }.ensuring(res => res.isFinite)

}