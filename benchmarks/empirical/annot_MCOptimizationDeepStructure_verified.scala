
package annot
import stainless.lang.*

// https://github.com/uzh/PPLib/blob/d55762e68f12dba9b3048d283d85d80f91094269/src/main/scala/ch/uzh/ifi/pdeboer/pplib/examples/optimizationSimulation/MCOptimizationSimulation.scala#L8
class MCOptimizeConstants(val bestAnswer: Int) {

  def answerDistance(answer: Int) = {
	if bestAnswer.toLong - answer.toLong < Int.MinValue then
		Int.MinValue
	else if bestAnswer.toLong - answer.toLong > Int.MaxValue then
		Int.MaxValue
	else
		bestAnswer - answer
	}
}

case class MCOptimizationResult(text: Int, costInCents: Int, const: MCOptimizeConstants) {

	// TO SPECIFY: 204
	def costFunctionResult: Double = {
		const.answerDistance(text) + costInCents.toDouble
	}.ensuring(res => !res.isNaN)
}