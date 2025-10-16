// https://github.com/alvarogimenez/g-code-utils/blob/7187172a58046bc35f7be73f96f5941713826f48/src/main/scala/com/gomezgimenez/gcode/utils/entities/geometry/BoundingBox.scala

package annot0

import stainless.lang.*

case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double) {
  require(left.isFinite && right.isFinite && top.isFinite && bottom.isFinite)
  require(left <= right)
  require(bottom <= top)

  // TO SPECIFY: 162
  def width: Double = {
    right - left
  }.ensuring(res => res >= 0)

  // TO SPECIFY: 163
  def height: Double = {
    top - bottom
  }.ensuring(res => res >= 0)

}