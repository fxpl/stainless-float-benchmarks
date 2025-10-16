package annot

import stainless.collection.*
import stainless.lang.*

// https://github.com/bearsroom/mxnet-augmented/blob/af4843b249e312014d54ce38545fcb4fa546d7db/scala-package/core/src/main/scala/ml/dmlc/mxnet/Optimizer.scala

class Optimizer(weightSet: Set[Int], specialized: Boolean = false) extends Serializable {

  // TO SPECIFY: 205
  protected def getWd(index: Int, wd: Float): Float = {
    require(index >= 0)
    require(wd.isFinite && wd >= 0)

    if (specialized) {
      if (weightSet.contains(index)) {
        wd
      } else {
        0f
      }
    } else {
      wd
    }
  }.ensuring(res => res.isFinite && res >= 0)
}