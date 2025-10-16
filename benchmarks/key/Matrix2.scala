import stainless.lang.*

/** 2x2 Matrix
  */
case class Matrix2(a: Double, b: Double, c: Double, d: Double) {

  // Determinant of the matrix
  def determinant: Double = a * d - b * c

  /** Transpose of the matrix
    */
  def transpose: Matrix2 = {
    Matrix2(a, c, b, d)
  }.ensuring(res =>
    res.determinant.isFinite ==> (res.determinant == determinant)
  )
}
