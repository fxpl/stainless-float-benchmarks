import stainless.annotation.*
import stainless.lang.*
import stainless.math.*

/** RL Circuit
  */
case class Circuit(
    maxVoltage: Double,
    frequency: Double,
    resistance: Double,
    inductance: Double
) {
  require(1.0 < maxVoltage && maxVoltage < 12.0)
  require(1.0 < frequency && frequency < 100.0)
  require(1.0 < resistance && resistance < 50.0)
  require(0.001 < inductance && inductance < 0.004)

  /** Impedance of the circuit
    */
  def impedance: Complex = {
    Complex(resistance, 2.0 * Math.PI * frequency * inductance)
  }.ensuring(res => res.imaginaryPart.isFinite && 0.0001 < res.imaginaryPart && res.imaginaryPart < 10.0)

  /** Current in the circuit
    */
  def current: Complex = {
    Complex(maxVoltage, 0.0) / impedance
  }.ensuring(res =>
    res.realPart.isFinite && res.imaginaryPart.isFinite && (res.realPart != 0.0 || res.imaginaryPart != 0.0)
  )

  /** Instantaneous current in the circuit
    */
  def instantCurrent(time: Double): Double = {
    require(0.0 < time && time < 300.0)

    val maxCurrent = sqrt(
      current.realPart * current.realPart + current.imaginaryPart * current.imaginaryPart
    )
    val theta = atan(current.imaginaryPart / current.realPart)

    maxCurrent * cos((2.0 * Math.PI * frequency * time) + theta)
  }.ensuring(_.isFinite)

  /** Instantaneous voltage in the circuit
    */
  def instantVoltage(time: Double): Double = {
    require(0.0 < time && time < 300.0)
    maxVoltage * cos(2.0 * Math.PI * frequency * time)
  }.ensuring(_.isFinite)
}

/** Complex numbers
  */
case class Complex(realPart: Double, imaginaryPart: Double) {

  /** Complex number addition
    */
  def +(c: Complex): Complex = {
    require(c.realPart.isFinite && realPart.isFinite && imaginaryPart.isFinite && c.imaginaryPart.isFinite)
    Complex(realPart + c.realPart, imaginaryPart + c.imaginaryPart)
  }.ensuring(res =>
    (-1.0 <= c.realPart && c.realPart <= 0.0
      && -1.0 < realPart && realPart <= 1.0
      && -2.0 < imaginaryPart && imaginaryPart < 2.0
      && -2.0 < c.imaginaryPart && c.imaginaryPart < 0.0)
        ==> !res.realPart.isInfinity
  )

  /** The original KeY benchmark had two contracts for `add()`.
    * Since we cannot add multiple contracts to one function in Stainless,
    * we create two versions of the function, one with the valid contracts,
    * and, one additionally including each invalid contract (here, only one invalid).
    */
  def add_with_invalid_contract(c: Complex): Complex = {
    require(c.realPart.isFinite && realPart.isFinite && imaginaryPart.isFinite && c.imaginaryPart.isFinite)
    Complex(realPart + c.realPart, imaginaryPart + c.imaginaryPart)
  }.ensuring(res =>
    ((-1.0 <= c.realPart && c.realPart <= 0.0
      && -1.0 < realPart && realPart <= 1.0
      && -2.0 < imaginaryPart && imaginaryPart < 2.0
      && -2.0 < c.imaginaryPart && c.imaginaryPart < 0.0)
        ==> !res.realPart.isFinite)
      && (res.realPart.isFinite && res.imaginaryPart.isFinite)
  )

  /** Complex number division
    */
  def /(c: Complex): Complex = {
    val denominator =
      (c.realPart * c.realPart) + (c.imaginaryPart * c.imaginaryPart)
    val divRealPart =
      ((this.realPart * c.realPart) + (this.imaginaryPart * c.imaginaryPart)) / denominator
    val divImaginaryPart =
      ((this.imaginaryPart * c.realPart) - (this.realPart * c.imaginaryPart)) / denominator

    Complex(divRealPart, divImaginaryPart)
  }.ensuring(res =>
    ((1.0 < c.realPart && c.realPart < 50.0 &&
      0.0001 < c.imaginaryPart && c.imaginaryPart < 10.0 &&
      1.0 < realPart && realPart < 12.0 &&
      imaginaryPart == 0.0) ==> (
      res.realPart.isFinite && res.imaginaryPart.isFinite
    )) &&
      ((c.realPart == 0.0 && c.imaginaryPart == 0.0) ==> res.realPart.isNaN)
  )

  /** Comparison of complex numbers
    */
  def compare(c: Complex): Double = {
    require(-1.0 <= c.realPart && c.realPart <= 0.0)
    require(-1.0 < realPart && realPart <= 1.0)
    require(-2.0 < imaginaryPart && imaginaryPart < 2.0)
    require(-2.0 < c.imaginaryPart && c.imaginaryPart < 0.0)

    if realPart == c.realPart then imaginaryPart - c.imaginaryPart
    else realPart - c.realPart
  }.ensuring(!_.isInfinity)

  /** Complex number reciprocal
    */
  def reciprocal: Complex = {
    require(realPart.isFinite)
    require(imaginaryPart.isFinite)

    val squareRadius = realPart * realPart + imaginaryPart * imaginaryPart
    Complex(realPart / squareRadius, -imaginaryPart / squareRadius)
  }.ensuring(res =>
    ((realPart != 0.0 && imaginaryPart != 0.0) ==> (!res.realPart.isNaN && !res.imaginaryPart.isNaN)) &&
      ((realPart == 0.0 && imaginaryPart == 0.0) ==> (res.realPart.isNaN && res.imaginaryPart.isNaN))
  )
}
