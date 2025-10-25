package annot0_verified

import stainless.lang.* 
import stainless.collection.*

// https://github.com/lmd1993/spark-branch-1.6/blob/b1a9c32c82e5154b3296348edefca8a3b782371d/mllib/src/main/scala/org/apache/spark/mllib/evaluation/AreaUnderCurve.scala

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
 * Computes the area under the curve (AUC) using the trapezoidal rule.
 */
object AreaUnderCurve {

  /**
   * Uses the trapezoidal rule to compute the area under the line connecting the two input points.
   * @param points two 2D points stored in Seq
   */
  // TO SPECIFY: 449
  private def trapezoid(points: Array[(Double, Double)]): Double = {
    require(points.length == 2)
    require(points(0)._1.isFinite && points(1)._1.isFinite)
    require(points(0)._2.isFinite && points(1)._2.isFinite)
    require(points(0)._1 < points(1)._1) 
    val x = points(0)
    val y = points(1)
    if (y._2 + x._2).isZero then 0.0 else
      (y._1 - x._1) * (y._2 + x._2) / 2.0
  }.ensuring(res => !res.isNaN)
}