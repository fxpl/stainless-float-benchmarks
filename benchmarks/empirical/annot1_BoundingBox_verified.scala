package annot1

import stainless.lang.*

// https://github.com/alvarogimenez/g-code-utils/blob/7187172a58046bc35f7be73f96f5941713826f48/src/main/scala/com/gomezgimenez/gcode/utils/entities/geometry/BoundingBox.scala

case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double) {
  require(left <= right)
  require(bottom <= top)

  // added:
  require(!(left - right).isNaN)
  require(!(bottom - top).isNaN)

  // TO SPECIFY: 161
  def width: Double = {
    right - left
  }.ensuring(!_.isNaN)

  // TO SPECIFY: 162
  def height: Double = {
    top - bottom
  }.ensuring(!_.isNaN)

}