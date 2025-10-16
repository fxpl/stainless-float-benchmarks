package annot_unknown

import stainless.collection.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/danitico/recsys-spark/blob/be39a0b6529c491e36aa553633979c9f20f08c3f/src/main/scala/similarity/JaccardSimilarity.scala
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