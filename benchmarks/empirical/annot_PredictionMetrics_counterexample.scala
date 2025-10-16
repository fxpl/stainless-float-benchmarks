package annot

import stainless.math.*
import stainless.collection.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/danitico/recsys-spark/blob/be39a0b6529c491e36aa553633979c9f20f08c3f/src/main/scala/metrics/PredictionMetrics.scala
class PredictionMetrics(_errors: List[Double]) {

  require(_errors.forall(x => !x.isNaN && x >= 0))

  // TO SPECIFY: 337
  private def getRMSE: Double = {
    map_forall(_errors, pow(_, 2), x => !x.isNaN && x >= 0, x => !x.isNaN && x >= 0)
    sqrt(
      this._errors.map(pow(_, 2)).sum / this._errors.isize
    )
  }.ensuring(0d <= _)

  // TO SPECIFY: 338
  private def getMAE: Double = {
    map_forall(_errors, abs, x => !x.isNaN && x >= 0, x => !x.isNaN && x >= 0)
    this._errors.map(abs).sum / this._errors.isize
  }.ensuring(0d <= _)
}