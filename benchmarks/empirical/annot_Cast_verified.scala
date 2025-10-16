package annot

// https://github.com/Lukas-Grasmann/skyline-queries-spark/blob/25a2f0773db0108b0bcc8f7756daf7f308da09f6/common/unsafe/src/main/java/org/apache/spark/sql/catalyst/util/DateTimeConstants.java#L20

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