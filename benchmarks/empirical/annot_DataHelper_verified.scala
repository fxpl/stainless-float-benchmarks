package annot

// https://github.com/CaiWeibo/incubator-mxnet-vbc/blob/3d5c9bedcc1ab6ab40eac69171df53b61d7d9bea/scala-package/examples/src/main/scala/org/apache/mxnetexamples/cnntextclassification/DataHelper.scala

import stainless.math.wrapping
import stainless.lang.*

object DataHelper {

  // TO SPECIFY: 49
  def getFloat(b: Array[Byte]): Float = {
    require(b.length == 4)
    var accum = 0
    accum = accum | (b(0) & 0xff) << 0
    accum = accum | (b(1) & 0xff) << 8
    accum = accum | (b(2) & 0xff) << 16
    accum = accum | (b(3) & 0xff) << 24
    java.lang.Float.intBitsToFloat(accum).toFloat
  }.ensuring(res => {
    val bits = java.lang.Float.floatToIntBits(res)
    (!res.isNaN || (b(3) & 0x7f) == 0x7f && (b(2) & 0x80) == 0x80 && ((b(2) & 0x7f) != 0 || b(1) != 0 || b(0) != 0))
      && (res.isNaN || wrapping(b(3) == (bits >> 24).toByte && b(2) == (bits >> 16).toByte && b(1) == (bits >> 8).toByte && b(0) == bits.toByte))
  })
}