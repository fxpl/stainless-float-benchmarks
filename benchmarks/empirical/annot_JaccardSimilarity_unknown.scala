package annot_unknown

import stainless.collection.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/danitico/recsys-spark/blob/be39a0b6529c491e36aa553633979c9f20f08c3f/src/main/scala/similarity/JaccardSimilarity.scala

/*
  similarity/JaccardSimilarity.scala
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

class JaccardSimilarity {
  // TO SPECIFY: 490
  def getSimilarity(firstArray: List[Double], secondArray: List[Double]): Double = {
    require(firstArray.isize == secondArray.isize)
    require(firstArray.isize > 0)

    val arrays = firstArray.zip(secondArray)

    val intersection = arrays.icount(tuple => !tuple._1.isNaN && !tuple._2.isNaN && tuple._1 == tuple._2 && tuple._1 == 1 && tuple._2 == 1).toDouble
    val union = arrays.icount(tuple =>  !(tuple._1 + tuple._2).isNaN && (tuple._1 + tuple._2) > 0).toDouble
    icountForall(
      arrays, 
      tuple => !tuple._1.isNaN && !tuple._2.isNaN && tuple._1 == tuple._2 && tuple._1 == 1 && tuple._2 == 1, 
      tuple => !(tuple._1 + tuple._2).isNaN && (tuple._1 + tuple._2) > 0
    )

    intersection / union
  }.ensuring(res => 0d <= res && res <= 1d)
}