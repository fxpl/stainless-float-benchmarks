package annot

import stainless.lang.*

// https://github.com/ACINQ/bitcoin-lib/blob/master/src/main/scala/fr/acinq/bitcoin/scalacompat/BtcAmount.scala#L10

case class Satoshi(private val underlying: Long) {
  def toLong: Long = underlying
}

// https://github.com/ACINQ/bitcoin-lib/blob/15173898d7025219cfd7ca04b28463b9020f8890/src/main/scala/fr/acinq/bitcoin/scalacompat/package.scala

/**
 * see https://en.bitcoin.it/wiki/Protocol_specification
 */

implicit object NumericSatoshi{
  // TO SPECIFY: 459
  def toDouble(x: Satoshi): Double = {
    x.toLong.toDouble
  }.ensuring(res => Long.MinValue <= res && res <= Long.MaxValue)
  // TO SPECIFY: 460
  def toFloat(x: Satoshi): Float = {
    x.toLong.toFloat
  }.ensuring(res => Long.MinValue <= res && res <= Long.MaxValue)
}