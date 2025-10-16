package annot

// https://github.com/dilettacal/ds_scala_big_data/blob/a3a025eb220948c726e02bbdb99a1beb0d226951/KlausurVorbereitung/src/main/scala/Intro.scala

import stainless.lang.*

object Intro{ 
  def square(x:Double) = x*x

  // TO SPECIFY: 300
  def sumOfSquares(x:Double, y:Double): Double = {
    require(!x.isNaN && !y.isNaN)
    square(x) + square(y)
  }.ensuring(0d <= _)
}