package annot

import stainless.lang.*

// https://github.com/imoonkin/Flink-State-Migration/blob/42cfd2ca34b7bc8ff25cfe6e97750147cd0a88b8/flink-table/flink-table-planner/src/main/scala/org/apache/flink/table/plan/stats/TableStats.scala

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