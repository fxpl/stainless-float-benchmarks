package annot0
//https://github.com/EmergentOrder/onnx-scala/blob/6c1d340e5e703717a4071375467a1da73d33c14f/common/src/main/scala/Float16.scala

/*
 * Copyright 2019 The Agate Authors
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
//Source: https://github.com/stripe/agate/blob/master/core/src/main/scala/com/stripe/agate/tensor/Float16.scala

import stainless.lang.*

/** Float16 represents 16-bit floating-point values.
  *
  * This type does not actually support arithmetic directly. The expected use case is to convert to
  * Float to perform any actual arithmetic, then convert back to a Float16 if needed.
  *
  * Binary representation:
  *
  * sign (1 bit) \| \| exponent (5 bits)
  * |  |
  * |:-|
  * |  |
  * mantissa (10 bits) \| | | x xxxxx xxxxxxxxxx
  *
  * Value interpretation (in order of precedence, with _ wild):
  *
  * 0 00000 0000000000 (positive) zero 1 00000 0000000000 negative zero _ 00000 __________ subnormal
  * number _ 11111 0000000000 +/- infinity _ 11111 __________ not-a-number _ _____ __________ normal
  * number
  *
  * An exponent of all 1s signals a sentinel (NaN or infinity), and all 0s signals a subnormal
  * number. So the working "real" range of exponents we can express is [-14, +15].
  *
  * For non-zero exponents, the mantissa has an implied leading 1 bit, so 10 bits of data provide 11
  * bits of precision for normal numbers.
  *
  * For normal numbers:
  *
  * x = (1 - sign*2) * 2^exponent * (1 + mantissa/1024)
  *
  * For subnormal numbers, the implied leading 1 bit is absent. Thus, subnormal numbers have the
  * same exponent as the smallest normal numbers, but without an implied 1 bit.
  *
  * So for subnormal numbers:
  *
  * x = (1 - sign*2) * 2^(-14) * (mantissa/1024)
  */
class Float16(val raw: Short) {

  def isNaN: Boolean  = (raw & 0x7fff) > 0x7c00

  /** Return the sign of a Float16 value as a Float.
   *
   * There are five possible return values:
   *
   * * NaN: the value is Float16.NaN (and has no sign) * -1F: the value is a non-zero negative
   * number * -0F: the value is Float16.NegativeZero * 0F: the value is Float16.Zero * 1F: the
   * value is a non-zero positive number
   *
   * PositiveInfinity and NegativeInfinity return their expected signs.
   */
  // TO SPECIFY: 176
  def signum: Float = {
    if (raw == -0x8000) -0f
    else if (raw == 0x0000) 0f
    else if (isNaN) Float.NaN
    else 1f - ((raw >>> 14) & 2)
  }.ensuring(res => isNaN && res.isNaN || 
                    res == -1 || res == 0 || res == 1)
}