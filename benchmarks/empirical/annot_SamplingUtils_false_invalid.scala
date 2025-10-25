package annot_verified

import stainless.lang.*
import stainless.math

// https://github.com/phenixmzy/spark-2.1.0/blob/bb535a80492de7ac696107559bf314b800dd9fc6/core/src/main/scala/org/apache/spark/util/random/StratifiedSamplingUtils.scala

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

object SamplingUtils {
 /**
   * Returns a sampling rate that guarantees a sample of size greater than or equal to
   * sampleSizeLowerBound 99.99% of the time.
   *
   * How the sampling rate is determined:
   *
   * Let p = num / total, where num is the sample size and total is the total number of
   * datapoints in the RDD. We're trying to compute q {@literal >} p such that
   *   - when sampling with replacement, we're drawing each datapoint with prob_i ~ Pois(q),
   *     where we want to guarantee
   *     Pr[s {@literal <} num] {@literal <} 0.0001 for s = sum(prob_i for i from 0 to total),
   *     i.e. the failure rate of not having a sufficiently large sample {@literal <} 0.0001.
   *     Setting q = p + 5 * sqrt(p/total) is sufficient to guarantee 0.9999 success rate for
   *     num {@literal >} 12, but we need a slightly larger q (9 empirically determined).
   *   - when sampling without replacement, we're drawing each datapoint with prob_i
   *     ~ Binomial(total, fraction) and our choice of q guarantees 1-delta, or 0.9999 success
   *     rate, where success rate is defined the same as in sampling with replacement.
   *
   * The smallest sampling rate supported is 1e-10 (in order to avoid running into the limit of the
   * RNG's resolution).
   *
   * @param sampleSizeLowerBound sample size
   * @param total size of RDD
   * @param withReplacement whether sampling with replacement
   * @return a sampling rate that guarantees sufficient sample size with 99.99% success rate
   */
  // TO SPECIFY: 470
  def computeFractionForSampleSize(sampleSizeLowerBound: Int, total: Long,
      withReplacement: Boolean): Double = {
    require(0 <= sampleSizeLowerBound && sampleSizeLowerBound <= total && 0 < total)
    if (withReplacement) {
      math.min(math.max(PoissonBounds.getUpperBound(sampleSizeLowerBound) / total, 1e-10), 1)
    } else {
      val fraction = sampleSizeLowerBound.toDouble / total
      BinomialBounds.getUpperBound(1e-4, total, fraction)
    }
  }.ensuring(res => 1e-10 <= res && res <= 1d)
}

/**
 * Utility functions that help us determine bounds on adjusted sampling rate to guarantee exact
 * sample sizes with high confidence when sampling with replacement.
 */
object PoissonBounds {

  /**
   * Returns a lambda such that Pr[X {@literal >} s] is very small, where X ~ Pois(lambda).
   */
  // TO SPECIFY: 471
  def getLowerBound(s: Double): Double = {
    require(s.isFinite && s >= 0)
    math.max(s - numStd(s) * math.sqrt(s), 1e-15)
  }.ensuring(res => 1e-15 <= res && res <= math.max(s, 1e-15))

  /**
   * Returns a lambda such that Pr[X {@literal <} s] is very small, where X ~ Pois(lambda).
   *
   * @param s sample size
   */
  // TO SPECIFY: 472
  def getUpperBound(s: Double): Double = {
    require(s.isFinite && s >= 0)
    math.max(s + numStd(s) * math.sqrt(s), 1e-10)
  }.ensuring(res => res.isFinite && math.max(s, 1e-10) <= res)

  // TO SPECIFY: 473
  private def numStd(s: Double): Double = {
    require(s.isFinite && s >= 0)
    // TODO: Make it tighter.
    if (s < 6.0) {
      12.0
    } else if (s < 16.0) {
      9.0
    } else {
      6.0
    }
  }.ensuring(res => res == 12.0 || res == 9.0 || res == 6.0)
}

/**
 * Utility functions that help us determine bounds on adjusted sampling rate to guarantee exact
 * sample size with high confidence when sampling without replacement.
 */
object BinomialBounds {

  val minSamplingRate = 1e-10

  /**
   * Returns a threshold `p` such that if we conduct n Bernoulli trials with success rate = `p`,
   * it is very unlikely to have more than `fraction * n` successes.
   */
  // TO SPECIFY: 474
  def getLowerBound(delta: Double, n: Long, fraction: Double): Double = {
    require(0 < n)
    require(!delta.isNaN && 0d <= delta && delta <= 1d)
    require(!fraction.isNaN && 0d <= fraction && fraction <= 1d)
    val gamma = - math.log(delta) / n * (2.0 / 3.0)
    if gamma.isPosInfinity then fraction else
    fraction + gamma - math.sqrt(gamma * gamma + 3 * (gamma * fraction))
  }.ensuring(res => !res.isNaN && 0d <= res && res <= 1)

  /**
   * Returns a threshold `p` such that if we conduct n Bernoulli trials with success rate = `p`,
   * it is very unlikely to have less than `fraction * n` successes.
   */
  // TO SPECIFY: 475
  def getUpperBound(delta: Double, n: Long, fraction: Double): Double = {
    require(0 < n)
    require(!delta.isNaN && 0d <= delta && delta <= 1d)
    require(!fraction.isNaN && 0d <= fraction && fraction <= 1d)
    val gamma = - math.log(delta) / n
    math.min(1,
      math.max(minSamplingRate, fraction + gamma + math.sqrt(gamma * gamma + 2 * gamma * fraction)))
  }.ensuring(res => minSamplingRate <= res && res <= 1)
}
