package annot_verified

import stainless.lang.*
import stainless.math


// https://github.com/wookietreiber/eva4s-old/blob/79e01c76b8ceb529a35987a4e28d534da6e925e9/core/main/scala/package.scala#L24

  /** A mutagen determines the probability with which individuals mutate, depending on the current
    * generation.
    *
    * @see [[mutating]]
    */
  type Mutagen = Int => Double

// https://github.com/wookietreiber/eva4s-old/blob/79e01c76b8ceb529a35987a4e28d534da6e925e9/core/main/scala/mutating/ExponentialMutagen.scala

/** A mutagen based on the monotonic function `f(x) = a * exp(b*x)`.
  *
  * @param generations the amount of intended generations
  * @param start the probability for the first generation
  * @param end the probability for the final generation
  */
case class ExponentialMutagen(generations: Int, start: Double = 0.8, end: Double = 0.01) extends Mutagen {
  require(0 <= generations)
  require(0d <= start && start <= 1d)
  require(0d <= end && end <= 1d)

  private val a = start

  private val c = end / start

  // TO SPECIFY: 125
  override def apply(generation: Int): Double = {
    require(0 <= generation && generation <= generations)

    if start.isZero then 0 else
    if end.isZero then 0 else 

      a * math.pow(c, generation.toDouble / generations)
  }.ensuring(res => math.min(start, end) <= res && res <= math.max(start, end))

}