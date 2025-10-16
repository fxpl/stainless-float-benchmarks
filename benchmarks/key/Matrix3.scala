import stainless.lang.*

/** 3x3 Matrix
  */
case class Matrix3(
    a: Double,
    b: Double,
    c: Double,
    d: Double,
    e: Double,
    f: Double,
    g: Double,
    h: Double,
    i: Double
) {

  // Determinant of the matrix
  def determinant: Double =
    (a * e * i + b * f * g + c * d * h) - (c * e * g + b * d * i + a * f * h)

  def determinantNew: Double =
    (a * (e * i) + (g * (b * f) + c * (d * h))) - (e * (c * g) + (i * (b * d) + a * (f * h)))

  /** Transpose of the matrix
    */
  def transposedEq: Matrix3 = {
    Matrix3(a, d, g, b, e, h, c, f, i)
  }.ensuring(res =>
    res.determinant.isFinite ==> (res.determinant == determinant)
  )

  def transposedEqV2: Matrix3 = {
    Matrix3(a, d, g, b, e, h, c, f, i)
  }.ensuring(res =>
    res.determinantNew.isFinite ==> (res.determinantNew == determinantNew)
  )

  def leftAssocDeterminant: Double =
    (a * e * i + b * f * g + c * d * h) - (c * e * g + b * d * i + a * f * h)

}

// /** Counterexample for the property that the determinant of a matrix is equal to
//   * the determinant of its transpose when the operations in the determinant are
//   * left associated
//   */
// def leftAssocDeterminantNotEqTranspose: Matrix3 = {
//   Matrix3(0.0, -3.33630883675144e-162, 1.340780797364698e154,
//     -3.33630883675144e-162, 4.9e-324, 6.365987373e-314, 1.340780797364698e154,
//     0.0, -8.988465732909575e307)
// }.ensuring(res =>
//   res.leftAssocDeterminant.isFinite &&
//     res.leftAssocDeterminant != res.transpose.leftAssocDeterminant
// )
