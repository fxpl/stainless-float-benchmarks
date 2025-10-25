package annot

import stainless.math
import stainless.lang.*
import stainless.annotation.*

// https://github.com/phenixmzy/spark-2.1.0/blob/bb535a80492de7ac696107559bf314b800dd9fc6/core/src/main/scala/org/apache/spark/util/StatCounter.scala

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

/**
 * A class for tracking the statistics of a set of numbers (count, mean and variance) in a
 * numerically robust way. Includes support for merging two StatCounters. Based on Welford
 * and Chan's <a href="http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance">
 * algorithms</a> for running variance.
 *
 * @constructor Initialize the StatCounter with the given values.
 */
class StatCounter(
    n: Long = 0, // Running count of our values
    mu: Double = 0, // Running mean of our values,
    m2: Double = 0, // Running variance numerator (sum of (x - mean)^2)
    ) extends Serializable {

  require(!mu.isNaN)
  require(n >= 0)
  require(!m2.isNaN && m2 >= 0)

  // TO SPECIFY: 304
  def sum: Double = {
    n * mu
  }.ensuring(res => !res.isNaN)

  /**
   * Return the population variance of the values.
   */
  // TO SPECIFY: 308
  @opaque
  def popVariance: Double = {
    if (n == 0) {
      Double.NaN
    } else {
      m2 / n
    }
  }.ensuring(res => res.isNaN || (n > 0 && 0 <= res && res <= m2 && (res.isFinite == m2.isFinite)))

  /**
   * Return the sample variance, which corrects for bias in estimating the variance by dividing
   * by N-1 instead of N.
   */
  // TO SPECIFY: 309
  @opaque
  def sampleVariance: Double = {
    if (n <= 1) {
      Double.NaN
    } else {
      m2 / (n - 1)
    }
  }.ensuring(res => res.isNaN || (n > 1 && 0 <= res && (res.isFinite == m2.isFinite)))

  /**
   * Return the population standard deviation of the values.
   */
  // TO SPECIFY: 311
  def popStdev: Double = {
    math.sqrt(popVariance)
  }.ensuring(res => res.isNaN || (n > 0 && 0 <= res && res <= math.sqrt(m2) && (res.isFinite == m2.isFinite)))
  /**
   * Return the sample standard deviation of the values, which corrects for bias in estimating the
   * variance by dividing by N-1 instead of N.
   */
  // TO SPECIFY: 312
  def sampleStdev: Double = {
    math.sqrt(sampleVariance)
  }.ensuring(res => res.isNaN || (n > 1 && 0 <= res && (res.isFinite == m2.isFinite)))

}