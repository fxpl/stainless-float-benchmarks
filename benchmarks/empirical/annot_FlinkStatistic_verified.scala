package annot

import stainless.lang.*

// https://github.com/imoonkin/Flink-State-Migration/blob/42cfd2ca34b7bc8ff25cfe6e97750147cd0a88b8/flink-table/flink-table-planner/src/main/scala/org/apache/flink/table/plan/stats/TableStats.scala

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

case class TableStats(rowCount: Long) {
  require(rowCount >= 0)
}

// https://github.com/imoonkin/Flink-State-Migration/blob/42cfd2ca34b7bc8ff25cfe6e97750147cd0a88b8/flink-table/flink-table-planner/src/main/scala/org/apache/flink/table/plan/stats/FlinkStatistic.scala

class FlinkStatistic(tableStats: Option[TableStats]) {
  /**
    * Returns the number of rows of the table.
    *
    * @return The number of rows of the table.
    */
  // TO SPECIFY: 149
  def getRowCount: Option[Double] = {
    tableStats match {
      case Some(tStats) => Some(tStats.rowCount.toDouble)
      case None() => None()
    }
  }.ensuring(res => res.isEmpty || 0 <= res.get && res.get <= Long.MaxValue)
}