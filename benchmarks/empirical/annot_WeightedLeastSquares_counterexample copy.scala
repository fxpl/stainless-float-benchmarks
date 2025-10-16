package annot

import stainless.lang.*
import stainless.math

// https://github.com/phenixmzy/spark-2.1.0/blob/bb535a80492de7ac696107559bf314b800dd9fc6/mllib/src/main/scala/org/apache/spark/ml/optim/WeightedLeastSquares.scala

/**
 * Aggregator to provide necessary summary statistics for solving [[WeightedLeastSquares]].
 */
private class Aggregator(bSum: Double = 0.0, bbSum: Double = 0.0, wSum: Double = 0.0) extends Serializable {
    require(!bSum.isNaN)
    require(!bbSum.isNaN && 0d <= bbSum)
    require(!wSum.isNaN && 0d <= wSum)

    // TO SPECIFY: 158
    def bBar: Double = {
        bSum / wSum
    }.ensuring(!_.isNaN)

    /**
     * Weighted mean of squared labels.
     */
    // TO SPECIFY: 159
    def bbBar: Double = {
        bbSum / wSum
    }.ensuring(0d <= _)

    /**
     * Weighted population standard deviation of labels.
     */
    // TO SPECIFY: 160
    def bStd: Double = {
        math.sqrt(bbSum / wSum - bBar * bBar)
    }.ensuring(0d <= _)
}