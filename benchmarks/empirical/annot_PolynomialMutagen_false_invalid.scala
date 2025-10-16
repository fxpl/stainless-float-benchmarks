package annot_false_invalid

import stainless.lang.*
import stainless.math

// https://github.com/wookietreiber/eva4s-old/blob/79e01c76b8ceb529a35987a4e28d534da6e925e9/core/main/scala/mutating/PolynomialMutagen.scala

/** A mutagen based on the monotonic function `f(x) = a + b * pow(x,degree)`.
  *
  * @param degree the polynomial degree of the function
  * @param generations the amount of intended generations
  * @param start the probability for the first generation
  * @param end the probability for the final generation
  */
case class PolynomialMutagen(degree: Double, generations: Int, start: Double = 0.8, end: Double = 0.01) {
  require(0 <= degree)
  require(0 <= generations)
  require(0d <= start && start <= 1d)
  require(0d <= end && end <= 1d)

  private val a = start

  private val b = ((end - start) / math.pow(generations, degree))

  // TO SPECIFY: 206
  def apply(generation: Int): Double = {
    require(0 <= generation && generation <= generations)
    if generations == 0 then start else a + b * math.pow(generation, degree)
  }.ensuring(res => math.min(start, end) <= res && res <= math.max(start, end))
}