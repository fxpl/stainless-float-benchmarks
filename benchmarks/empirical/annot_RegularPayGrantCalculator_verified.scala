package annot

import stainless.lang.*

/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//https://github.com/hmrc/coronavirus-jss-calculator-frontend/blob/8e3cf4c1ec709a5cf5c76d410c7c8543ebac042f/app/models/PayFrequency.scala#L24
sealed trait PayFrequency

case object Weekly extends PayFrequency

case object FortNightly extends PayFrequency

case object FourWeekly extends PayFrequency

case object Monthly extends PayFrequency

//https://github.com/hmrc/coronavirus-jss-calculator-frontend/blob/8e3cf4c1ec709a5cf5c76d410c7c8543ebac042f/app/services/RegularPayGrantCalculator.scala

trait RegularPayGrantCalculator {

  // TO SPECIFY: 339
  private def calculateAdjustedReferencePay(
    referencePay: Double,
    daysInPeriod: Int,
    qualifyingDaysInPeriod: Int
  ): Double = {
    require(referencePay.isFinite && 0.0 <= referencePay && referencePay <= 3125.00)
    require(0 < daysInPeriod && daysInPeriod <= 31)
    require(daysInPeriod >= qualifyingDaysInPeriod && qualifyingDaysInPeriod >= 0)
    (referencePay / daysInPeriod.toDouble) * qualifyingDaysInPeriod.toDouble
  }.ensuring(res => 0 <= res && res.isFinite)

  // TO SPECIFY: 340
  private def proportionReferencePay(referencePay: Double, daysInPeriod: Int, qualifyingDaysInPeriod: Int): Double = {
    require(referencePay.isFinite && 0.0 <= referencePay && referencePay <= 3125.00)
    require(0 < daysInPeriod && daysInPeriod <= 31)
    require(daysInPeriod >= qualifyingDaysInPeriod && qualifyingDaysInPeriod > 0)
    referencePay * (daysInPeriod.toDouble / qualifyingDaysInPeriod.toDouble)
  }.ensuring(res => res >= 0 && res.isFinite)

  // TO SPECIFY: 341
  private def calculateReferencePayCap(
    daysInPeriod: Int,
    isPartialPeriod: Boolean,
    payFrequency: PayFrequency
  ): Double = {
    require(0 < daysInPeriod && daysInPeriod <= 31)
    if (isPartialPeriod) daysInPeriod * 102.74
    else RegularPayGrantCalculator.fullPeriodPayCaps(payFrequency)
  }.ensuring(res => 0 <= res && res <= 3184.94)

}

object RegularPayGrantCalculator {

  def fullPeriodPayCaps: PayFrequency => Double = {
    case Weekly      => 721.15
    case FortNightly => 1442.30
    case FourWeekly  => 2884.60
    case Monthly     => 3125.00
  }

}