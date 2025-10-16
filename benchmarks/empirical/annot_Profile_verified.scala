package annot_verified

import stainless.lang.*

// https://github.com/alexishorner/dotty/blob/eef332ee725776eb445de57801e6a026a7da48b3/compiler/src/dotty/tools/dotc/reporting/Profile.scalaobject Profile:

object Profile:
  inline val TastyChunkSize = 50

  def chunks(size: Int) = {
    require(0 <= size)
    size / TastyChunkSize + ((TastyChunkSize - 1) / TastyChunkSize)
  }.ensuring(res => res >= 0)

  class Info(lineCount: Int = 0, tastySize: Int = 0):
    require(0 <= tastySize)
    require(lineCount >= 0)

    // TO SPECIFY: 272
    def complexity: Float = {
      val c = chunks(tastySize)
      if c == 0 then 0f else
      c.toFloat/lineCount
    }.ensuring(res => res >= 0)