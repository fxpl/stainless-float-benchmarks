package annot

// https://github.com/Lukas-Grasmann/skyline-queries-spark/blob/25a2f0773db0108b0bcc8f7756daf7f308da09f6/common/unsafe/src/main/java/org/apache/spark/sql/catalyst/util/DateTimeConstants.java#L20

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

object DateTimeConstants {
  val MILLIS_PER_SECOND: Long = 1000L
  val MICROS_PER_MILLIS: Long = 1000L
  val MICROS_PER_SECOND: Long = MILLIS_PER_SECOND * MICROS_PER_MILLIS
}

// https://github.com/Lukas-Grasmann/skyline-queries-spark/blob/25a2f0773db0108b0bcc8f7756daf7f308da09f6/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/Cast.scala

// TO SPECIFY: 292
def timestampToDouble(ts: Long): Double = {
  require(0 <= ts)
  ts / DateTimeConstants.MICROS_PER_SECOND.toDouble
}.ensuring(res => 0d <= res && res <= Long.MaxValue.toDouble / DateTimeConstants.MICROS_PER_SECOND)