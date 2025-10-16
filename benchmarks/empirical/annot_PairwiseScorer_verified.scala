package annot

import stainless.collection.*
import stainless.lang.*
import utils.Utils.*

// https://github.com/lephong/diffmetric_coref/blob/37543d5000911917f86a3c1747a50aec65b53c2d/modifiedBCS/src/main/java/edu/berkeley/nlp/coref/PairwiseScorer.scala

class PairwiseScorer(val weights: Array[Double]) extends Serializable {
  require(forall((i: Int) => (0 <= i && i < weights.length) ==> weights(i).isFinite))

  def numWeights = weights.length

  // TO SPECIFY: 482
  def scoreIndexedFeats(feats: Array[Int]): Double = {
    require(forall((i: Int) => (0 <= i && i < feats.length)  ==> (0 <= feats(i) && feats(i) < numWeights)))
    var featIdx = 0;
    var featTotal = 0.0;
    (while (featIdx < feats.length) {
      decreases(feats.length - featIdx)
      featTotal += weights(feats(featIdx));
      featIdx += 1;
    }).invariant(
      0 <= featIdx && featIdx <= feats.length && !featTotal.isNaN
    )
    featTotal
  }.ensuring(!_.isNaN)
}