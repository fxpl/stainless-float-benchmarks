package annot

import stainless.lang.*

// https://github.com/outr/youi/blob/6c81b1262569cdaba4e81681362d909022fe3a8e/gui/src/main/scala/io/youi/path/Rectangle.scala

case class Rectangle(x: Double,
                     y: Double,
                     width: Double,
                     height: Double) {


  require(width >= 0.0)
  require(height >= 0.0)
  require(x.isFinite && y.isFinite && width.isFinite && height.isFinite)

  def left: Double = x
  def top: Double = y

  // TO SPECIFY: 434
  def right: Double = {
    x + width
  }.ensuring(res => res >= left)

  // TO SPECIFY: 435
  def bottom: Double = {
    y + height
  }.ensuring(res => res >= top)

  // TO SPECIFY: 436
  def center: Double = {
    x + (width / 2.0)
  }.ensuring(res => left <= res && res <= right)

  // TO SPECIFY: 437
  def middle: Double = {
    y + (height / 2.0)
  }.ensuring(res => top <= res && res <= bottom)

}