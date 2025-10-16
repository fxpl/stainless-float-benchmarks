package annot0

// https://github.com/shmkhaled/MoMatch/blob/e3fa54365412e47b160a088082d04fb9d55abacf/src/main/scala/OntologyStatistics.scala

import stainless.math
import stainless.lang.* 

object OntologyStatistics {
  // TO SPECIFY: 458
    def roundNumber(num: Double): Double = {
        require(!num.isNaN)
        math.rint(num * 100) / 100.toDouble
  }.ensuring(res => !num.isNaN && (num >= 0 ==> res >= 0)  && (num <= 0 ==> res <= 0))
}