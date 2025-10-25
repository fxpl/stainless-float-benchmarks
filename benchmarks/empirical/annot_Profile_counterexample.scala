package annot_counterexample

import stainless.lang.*

// https://github.com/alexishorner/dotty/blob/eef332ee725776eb445de57801e6a026a7da48b3/compiler/src/dotty/tools/dotc/reporting/Profile.scala

object Profile:
  inline val TastyChunkSize = 50

  // Associativity matters!!!
  def chunks(size: Int) = {
    require(0 <= size)
    (size + (TastyChunkSize - 1)) / TastyChunkSize
  }.ensuring(res => res >= 0)

  class Info(lineCount: Int = 0, tastySize: Int = 0):
    require(0 <= tastySize)
    require(lineCount >= 0)

    // TO SPECIFY: 272
    def complexity: Float = {
      chunks(tastySize).toFloat/lineCount
    }.ensuring(res => res >= 0)