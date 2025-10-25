package annot1

import stainless.math
import stainless.lang.* 

// https://github.com/shmkhaled/MoMatch/blob/e3fa54365412e47b160a088082d04fb9d55abacf/src/main/scala/OntologyStatistics.scala

object OntologyStatistics {
  // TO SPECIFY: 458
  def roundNumber(num: Double): Double = {
      math.rint(num * 100) / 100.toDouble
  }.ensuring(res => !res.isFinite || math.abs(res - num) <= math.abs(num))
}