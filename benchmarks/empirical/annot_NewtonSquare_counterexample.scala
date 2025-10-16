package annot

// https://github.com/dilettacal/ds_scala_big_data/blob/a3a025eb220948c726e02bbdb99a1beb0d226951/coursera/src/main/scala/NewtonSquare.scala

import stainless.lang.*
import stainless.math
import stainless.annotation.* 

object NewtonSquare {

  // Fails when applied to 0
  // Does not necessarily terminate

  /* Calculates the square root of parameter x
    * Successive approximation by Newton.
    * */
  // TO SPECIFY: 175
  def sqrt(x: Double): Double = {
    require(x.isFinite && x > 0)
    def isGoodEnough(estimation: Double):Boolean = {
      require(estimation.isFinite && estimation > 0)
      abs(estimation*estimation - x) / x < 0.001
    }
    def improve(d: Double): Double ={
      require(d.isFinite && d > 0)
      require(!isGoodEnough(d))
      (d + x / d)/2
    }.ensuring(res => 
      res.isFinite && res > 0 && (math.abs(math.sqrt(x) - res) <= math.abs(math.sqrt(x) - d))
    )

    def abs(x:Double): Double = {
      require(x.isFinite)
      if(x>0) x else -x
    }.ensuring(res => res == math.abs(x))
        
    def sqrtIter(estimation: Double): Double ={
      require(estimation.isFinite && estimation > 0)
      if(isGoodEnough(estimation)) estimation
      else sqrtIter(improve(estimation))
    }.ensuring(res => res >= 0 && abs(res * res - x) / x < 0.001)
    val res = sqrtIter(1)
    assert(math.abs(res * res - x) == abs(res * res - x))
    res
  }.ensuring(res => res >= 0 && math.abs(res * res - x) / x < 0.001)
  



//   println(sqrt(4))
//   println(sqrt(1e-6)) //not precise for very small numbers
//   print(sqrt(1e60)) //not terminating!
}