package annot_verified

// https://github.com/lichess-org/lila/blob/df66483def8cc81e5fea9aa37023a09b7cd09874/modules/storm/src/main/StormDay.scala

// stores data of the best run of the day
// plus the number of runs
case class StormDay(
    moves: Int,
    errors: Int
):

  require(moves >= errors)
  require(errors >= 0)

  // TO SPECIFY: 293
  // Associativity matters!!!
  def accuracyPercent: Float = {
    if moves == 0 then 100f else 100 * ((moves - errors) / moves.toFloat)
  }.ensuring(res => 0 <= res && res <= 100)