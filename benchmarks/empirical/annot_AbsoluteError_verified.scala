package annot

import stainless.math
import stainless.lang.*

// https://github.com/phenixmzy/spark-2.1.0/blob/bb535a80492de7ac696107559bf314b800dd9fc6/mllib/src/main/scala/org/apache/spark/mllib/tree/loss/AbsoluteError.scala

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
 * :: DeveloperApi ::
 * Class for absolute error loss calculation (for regression).
 *
 * The absolute (L1) error is defined as:
 *  |y - F(x)|
 * where y is the label and F(x) is the model prediction for features x.
 */
object AbsoluteError {

  // TO SPECIFY: 165
  def computeError(prediction: Double, label: Double): Double = {
    require(prediction.isFinite && label.isFinite)
    val err = label - prediction
    math.abs(err)
  }.ensuring(res => res >= 0)
}