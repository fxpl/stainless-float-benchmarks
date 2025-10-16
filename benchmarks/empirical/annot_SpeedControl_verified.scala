package annot

// https://github.com/iyyel/conways-game-of-life/blob/9531117880d5d9abe184a55e8a83652138b0ecae/src/main/scala/io/iyyel/game/of/life/controls/SpeedControl.scala

class SpeedControl:

  // TO SPECIFY: 291
  def speedToTimes(speed: Int): Double = {
    require(1 <= speed && speed < 8)
    speed match
      case 1 => 0.10
      case 2 => 0.25
      case 3 => 0.50
      case 4 => 1.00
      case 5 => 2.00
      case 6 => 4.00
      case 7 => 8.00
  }.ensuring(res => res == 0.10d || res == 0.25d || res == 0.5d || res == 1d || res == 2d || res == 4d || res == 8d)