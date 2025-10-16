import stainless.lang.*

/** Absolute value of a floating point number
  */
def abs(v: Double): Double = if (v < 0) -v else v

case class Rectangle(x: Double, y: Double, width: Double, height: Double) {

  /** Scale the rectangle by the specified factors
    */
  def scale(arg1: Double, arg2: Double): Rectangle = {
    require(x.isFinite)
    require(y.isFinite)
    require(arg1.isFinite)
    require(arg2.isFinite)
    Area(this).transformedArea(scaling(arg1, arg2)).rectangle
  }.ensuring(res =>
    (((-3.38 <= res.x && res.x <= -5.53) &&
      (-3.38 <= res.y && res.y <= -5.53) &&
      (3.1 < res.width && res.width <= 3.7332) &&
      (3.0000001 < res.height && res.height <= 4.0004) &&
      (3.0003001 < arg1 && arg1 <= 4.0024) &&
      (-6.4000003 < arg2 && arg2 <= 3.0001)) ==>
      (res.x.isFinite && res.y.isFinite && res.width.isFinite && res.height.isFinite))
      // &&
      // (!res.x.isNaN && !res.y.isNaN && !res.width.isNaN && !res.height.isNaN)
  )

  /** The original KeY benchmark had two contracts for `add()`.
    * Since we cannot add multiple contracts to one function in Stainless,
    * we create two versions of the function, one with the valid contracts,
    * and, one additionally including each invalid contract (here, only one invalid).
    */
  def scale_with_invalid_contract(arg1: Double, arg2: Double): Rectangle = {
    require(x.isFinite)
    require(y.isFinite)
    require(arg1.isFinite)
    require(arg2.isFinite)
    Area(this).transformedArea(scaling(arg1, arg2)).rectangle
  }.ensuring(res =>
    (((-3.38 <= res.x && res.x <= -5.53) &&
      (-3.38 <= res.y && res.y <= -5.53) &&
      (3.1 < res.width && res.width <= 3.7332) &&
      (3.0000001 < res.height && res.height <= 4.0004) &&
      (3.0003001 < arg1 && arg1 <= 4.0024) &&
      (-6.4000003 < arg2 && arg2 <= 3.0001)) ==>
      (res.x.isFinite && res.y.isFinite && res.width.isFinite && res.height.isFinite))
      &&
      (!res.x.isNaN && !res.y.isNaN && !res.width.isNaN && !res.height.isNaN)
  )
}

/** From
  * https://docs.oracle.com/javase/7/docs/api/java/awt/geom/AffineTransform.html
  * The AffineTransform class represents a 2D affine transform that performs a
  * linear mapping from 2D coordinates to other 2D coordinates that preserves
  * the "straightness" and "parallelness" of lines. [ x'] [ m00 m01 m02 ] [ x ]
  * [ m00x + m01y + m02 ] [ y'] = [ m10 m11 m12 ] [ y ] = [ m10x + m11y + m12 ]
  * [ 1 ] [ 0 0 1 ] [ 1 ] [ 1 ]
  */
case class AffineTransform(
    m00: Double,
    m10: Double,
    m01: Double,
    m11: Double,
    m02: Double,
    m12: Double
)

/** Creates a scaling transformation
  */
def scaling(sx: Double, sy: Double): AffineTransform = {
  AffineTransform(sx, 0.0, 0.0, sy, 0.0, 0.0)
}

case class Area(rectangle: Rectangle) {

  // Creates a new Area object that contains the same geometry as this Area transformed by the specified AffineTransform
  // In this case creates a scaled rectangle based on v2
  def transformedArea(v2: AffineTransform): Area = {

    val oldUpperRightPointX = rectangle.x + rectangle.width
    val oldUpperRightPointY = rectangle.y

    val oldLowerLeftPointX = rectangle.x
    val oldLlowerLeftPointY = rectangle.y - rectangle.height

    val newX = (rectangle.x * v2.m00) + (rectangle.y * v2.m01) + v2.m02
    val newY = (rectangle.x * v2.m10) + (rectangle.y * v2.m11) + v2.m12

    val newUpperRightPointX =
      (oldUpperRightPointX * v2.m00) + (oldUpperRightPointY * v2.m01) + v2.m02
    val newLowerLeftPointY =
      (oldLowerLeftPointX * v2.m10) + (oldLlowerLeftPointY * v2.m11) + v2.m12

    Area(
      Rectangle(
        newX,
        newY,
        abs(newX - newUpperRightPointX),
        abs(newY - newLowerLeftPointY)
      )
    )
  }

}
