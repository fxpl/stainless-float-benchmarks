package annot

import stainless.lang.* 
import stainless.math

// https://github.com/lmd1993/spark-branch-1.6/blob/b1a9c32c82e5154b3296348edefca8a3b782371d/mllib/src/main/scala/org/apache/spark/mllib/clustering/LDAOptimizer.scala

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

class OnlineLDAOptimizer(var tau0: Double = 1024, var kappa: Double = 0.51, var iteration: Int = 0) {
  require(iteration >= 0)
  require(kappa.isFinite && kappa > 0)
  require(tau0.isFinite && tau0 > 0)

  /**
   * A (positive) learning parameter that downweights early iterations. Larger values make early
   * iterations count less.
   */
  def getTau0: Double = this.tau0

  /**
   * Learning rate: exponential decay rate
   */
  def getKappa: Double = this.kappa

  // TO SPECIFY: 394
  /** Calculate learning rate rho for the current [[iteration]]. */
  private def rho(): Double = {
    math.pow(getTau0 + this.iteration, -getKappa)
  }.ensuring(res => res >= 0)

}
