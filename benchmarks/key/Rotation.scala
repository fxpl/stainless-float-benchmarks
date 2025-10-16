object Rotation {

  val cos90: Double = 6.123233995736766e-17;
  val sin90: Double = 1.0;

  /** Rotate a 2D vector by 90 degrees
    */
  def rotate(vec: (Double, Double)): (Double, Double) = {
    val x = vec._1 * cos90 - vec._2 * sin90
    val y = vec._1 * sin90 + vec._2 * cos90
    (x, y)
  }

  /** Absolute value of a floating point number
    */
  def abs(v: Double): Double = if (v < 0) -v else v

  /** Absolute error between rotating a vector 4 times and the original vector
    */
  def error(vec: (Double, Double)): (Double, Double) = {
    require(1.0 < vec._1 && vec._1 < 2.0)
    require(1.0 < vec._2 && vec._2 < 2.0)

    val temp = rotate(rotate(rotate(rotate(vec))))
    (abs(temp._1 - vec._1), abs(temp._2 - vec._2))
  }.ensuring(res =>
    -1.0e-15 < res._1 && res._1 < 1.0e-15 &&
      -1.0e-15 < res._2 && res._2 < 1.0e-15
  )

  /** Relative error between rotating a vector 4 times and the original vector
    */
  def relError(vec: (Double, Double)): (Double, Double) = {
    require(1.0 < vec._1 && vec._1 < 2.0)
    require(1.0 < vec._2 && vec._2 < 2.0)
    (error(vec)._1 / vec._1, error(vec)._2 / vec._2)
  }.ensuring(res =>
    -1.0e-15 < res._1 && res._1 < 1.0e-15 &&
      -1.0e-15 < res._2 && res._2 < 1.0e-15
  )
}
