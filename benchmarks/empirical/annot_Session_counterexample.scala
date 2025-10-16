package annot_session

import stainless.lang.*
import stainless.math

// https://github.com/zfwf/progfun1/blob/a77f1dd494d579000cd3ede94876606887b1fa7a/week1/src/main/scala/example/Session.scala

// TO SPECIFY: 326
def sqrt(x: Double): Double = {
    require(x.isFinite && x > 0)
    def abs(x: Double) = {
      require(!x.isNaN)
      if (x < 0) -x else x
    }.ensuring(res => res == math.abs(x))

    def sqrtIter(guess: Double, x: Double): Double = {
      require(guess.isFinite && guess > 0)
      require(x.isFinite && x > 0)
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    }.ensuring(res => res >= 0 && abs(res * res - x) / x < 0.001)

    def isGoodEnough(guess: Double, x: Double): Boolean =
      require(guess.isFinite && guess > 0)
      require(x.isFinite && x > 0)
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double): Double = {
      require(x.isFinite && x > 0)
      require(guess.isFinite && guess > 0)
      require(!isGoodEnough(guess, x))
      (guess + x / guess) / 2
    }.ensuring(res => 
      res.isFinite && res > 0 && (math.abs(math.sqrt(x) - res) <= math.abs(math.sqrt(x) - guess))
    )

    val res = sqrtIter(1, x)
    res
  }.ensuring(res => 0 <= res && math.abs(res * res - x) / x < 0.001)