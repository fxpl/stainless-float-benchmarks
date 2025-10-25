package annot

import stainless.math.*
import stainless.collection.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/danitico/recsys-spark/blob/be39a0b6529c491e36aa553633979c9f20f08c3f/src/main/scala/metrics/PredictionMetrics.scala

/*
  metrics/PredictionMetrics.scala
  Copyright (C) 2022 Daniel Ranchal Parrado <danielranchal@correo.ugr.es>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>
*/

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