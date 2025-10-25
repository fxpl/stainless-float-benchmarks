package annot_verified

import stainless.lang.*
import stainless.math.*

// https://github.com/orisano/fpinscala/blob/d9adad051bebe25f9b86a5994daa049b58005c3c/exercises/src/main/scala/fpinscala/streamingio/StreamingIO.scala

  /*
    * Exercise 9: Write a program that reads degrees fahrenheit as `Double` values from a file,
    * converts each temperature to celsius, and writes results to another file.
    */
  // TO SPECIFY: 491
  def toCelsius(fahrenheit: Double): Double = {
    require(fahrenheit.isFinite && -459.67 <= fahrenheit)
    max(-273.15, (5.0 / 9.0) * (fahrenheit - 32.0))
  }.ensuring(res => res.isFinite && -273.15 <= res)