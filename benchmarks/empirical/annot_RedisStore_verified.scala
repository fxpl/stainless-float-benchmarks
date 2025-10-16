package annot

import stainless.lang.*

// https://github.com/addb-swstarlab/spark-addb-connector/blob/23c48771fc1e88a84434d098a5213f76739314b7/src/main/scala/kr/ac/yonsei/delab/addb_srconnector/RedisStore.scala
class RedisStore {
  // TO SPECIFY: 448
  def _calculateDurationSec(start: Double, end: Double): Double = {
    require(start.isFinite && end.isFinite && start <= end)
    require(0 <= start)
    (end - start) / 1000.0f;
  }.ensuring(res => res.isFinite && 0 <= res)
}