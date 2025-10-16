package annot1

import stainless.annotation.*
import stainless.math
import stainless.lang.*
import utils.Utils.System

// https://github.com/Pinapse/giant/blob/04b79b7f2cc0934d81d9bcae47aa41880e134886/src/main/scala/net/psforever/types/CapacitorStateType.scala#L3
sealed trait CapacitorStateType

object CapacitorStateType {

  case object Idle extends CapacitorStateType
  case object Charging extends CapacitorStateType
  case object ChargeDelay extends CapacitorStateType
  case object Discharging extends CapacitorStateType
}


// https://github.com/Pinapse/giant/blob/04b79b7f2cc0934d81d9bcae47aa41880e134886/src/main/scala/net/psforever/objects/definition/ExoSuitDefinition.scala#L19

class ExoSuitDefinition(val maxCapacitor: Int = 0) {
  require(0 <= maxCapacitor)
}


// https://github.com/Pinapse/giant/blob/04b79b7f2cc0934d81d9bcae47aa41880e134886/src/main/scala/net/psforever/objects/Player.scala#L349

class Player(
  exosuit: ExoSuitDefinition, 
  var capacitor: Float = 0f,
  var capacitorLastUsedMillis: Long = 0, 
  var capacitorLastChargedMillis: Long = 0,
  var capacitorState: CapacitorStateType = CapacitorStateType.Idle) {
  require(!capacitor.isNaN && 0d <= capacitor)
  require(0 <= capacitorLastUsedMillis)
  require(0 <= capacitorLastChargedMillis)

  // TO SPECIFY: 493
  def Capacitor_=(value: Float): Float = {

    val newValue = math.min(math.max(0, value), exosuit.maxCapacitor.toFloat)

    if (newValue < capacitor) {
      capacitorLastUsedMillis = System.currentTimeMillis()
      capacitorLastChargedMillis = 0
    } else if (newValue > capacitor && newValue < exosuit.maxCapacitor) {
      capacitorLastChargedMillis = System.currentTimeMillis()
      capacitorLastUsedMillis = 0
    } else if (newValue > capacitor && newValue == exosuit.maxCapacitor) {
      capacitorLastChargedMillis = 0
      capacitorLastUsedMillis = 0
      capacitorState = CapacitorStateType.Idle
    }

    capacitor = newValue
    capacitor
  }.ensuring(res =>
    0d <= res && res <= exosuit.maxCapacitor.toFloat
      && (value.isNaN || !(0d <= value && value <= exosuit.maxCapacitor.toFloat) || res == value) // could also state when res is 0 and max
      && (!(res > old(this).capacitor && res == exosuit.maxCapacitor) || (capacitorLastUsedMillis == 0d && capacitorLastChargedMillis == 0d && capacitorState == CapacitorStateType.Idle))
      && (res <= old(this).capacitor || capacitorLastUsedMillis == 0d)
      && (res >= old(this).capacitor || capacitorLastChargedMillis == 0d)
  )
}
