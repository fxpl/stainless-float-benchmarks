package annot0_kmath

import stainless.math
import stainless.lang.*

// https://github.com/litan/kojo-lib/blob/4083e51bba1ca6b1fb09c5f4711fd277f06910e2/src/main/scala/net/kogics/kojo/core/shapes.scala#L39
/*
 * Copyright (C) 2010 Lalit Pant <pant.lalit@gmail.com>
 *
 * The contents of this file are subject to the GNU General Public License
 * Version 3 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.gnu.org/copyleft/gpl.html
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 */

class Point(val x: Double, val y: Double) {
  require(x.isFinite && y.isFinite)
}

// https://github.com/litan/kojo-lib/blob/4083e51bba1ca6b1fb09c5f4711fd277f06910e2/src/main/scala/net/kogics/kojo/kmath/Kmath.scala

object KMath {
  // TO SPECIFY: 349
  def distance(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    require(x1.isFinite && y1.isFinite && x2.isFinite && y2.isFinite) 
    math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
  }.ensuring(res => res >= 0)

  // TO SPECIFY: 350
  def distance(p1: Point, p2: Point): Double = {
    distance(p1.x, p1.y, p2.x, p2.y)
  }.ensuring(res => res >= 0)

  // TO SPECIFY: 351
  def angle(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    require(x1.isFinite && y1.isFinite && x2.isFinite && y2.isFinite)
    math.atan2(y2 - y1, x2 - x1).toDegrees
  }.ensuring(res => -180 <= res && res <= 180)

  // TO SPECIFY: 352
  def angle(p1: Point, p2: Point): Double = {
    require(p1.x.isFinite && p1.y.isFinite && p2.x.isFinite && p2.y.isFinite)
    angle(p1.x, p1.y, p2.x, p2.y)
  }.ensuring(res => -180 <= res && res <= 180)
}