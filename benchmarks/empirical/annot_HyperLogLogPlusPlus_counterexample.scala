package annot
import stainless.math
import stainless.lang.*

/**
 * HyperLogLog++ (HLL++) is a state of the art cardinality estimation algorithm. This class
 * implements the dense version of the HLL++ algorithm as an Aggregate Function.
 *
 * This implementation has been based on the following papers:
 * HyperLogLog: the analysis of a near-optimal cardinality estimation algorithm
 * http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf
 *
 * HyperLogLog in Practice: Algorithmic Engineering of a State of The Art Cardinality Estimation
 * Algorithm
 * http://static.googleusercontent.com/external_content/untrusted_dlcp/research.google.com/en/us/pubs/archive/40671.pdf
 *
 * Appendix to HyperLogLog in Practice: Algorithmic Engineering of a State of the Art Cardinality
 * Estimation Algorithm
 * https://docs.google.com/document/d/1gyjfMHy43U9OWBXxfaeG-3MjGzejW1dlpyMwEYAAWEI/view?fullscreen#
 */


//https://github.com/lmd1993/spark-branch-1.6/blob/b1a9c32c82e5154b3296348edefca8a3b782371d/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/aggregate/HyperLogLogPlusPlus.scala

case class HyperLogLogPlusPlus(relativeSD: Double = 0.05) {

  require(relativeSD.isFinite && relativeSD > 0d)

  private[this] val p = math.ceil(2.0d * math.log(1.106d / relativeSD) / math.log(2.0d)).toInt

  private[this] val m = 1 << p

  // TO SPECIFY: 225
  def trueRsd: Double = {
    1.04 / math.sqrt(m)
  }.ensuring(res => res > 0)

}