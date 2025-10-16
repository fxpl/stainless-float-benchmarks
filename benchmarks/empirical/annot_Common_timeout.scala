package annot

import stainless.annotation.*
import stainless.lang.*
import stainless.math
import stainless.collection.*

// https://github.com/wangshusen/SparkGiant/blob/90cf9d9f0ad7fa39a434e9a66eda752c3561b88b/src/main/scala/quadratic/Common.scala

/**
 * Constants common to driver and executors
 */
object Constants {
    val numStepSizes: Int = 3
    val baseStepSizes: Double = 4.0
}


/**
 * Solve a ridge regression problem. 
 * Model: 0.5*||X w - y||_2^2 + 0.5*gamma*||w||_2^2
 */
class Driver(objVal: Double = 1.0E10) {
    require(objVal.isFinite)
    
    // for line search
    val numStepSizes: Int = Constants.numStepSizes
    val baseStepSizes: Double = Constants.baseStepSizes
    val stepSizes: List[Double] = indices(0, numStepSizes).map(1.0 / math.pow(baseStepSizes, _))

    /** 
     * Search for the best step size eta
     *
     * @param objVals array of objective values
     * @param pg = -0.1 * <p, g>
     * @return eta the biggest step size that leads to sufficient improvement
     */
    // TO SPECIFY: 25
    def lineSearch(objVals: Array[Double], pg: Double): Double = {
        require(objVals.length == numStepSizes)

        def rec(j: Int, eta: Double): Double = {
            decreases(numStepSizes - j)
            require(0 <= j && j <= numStepSizes)
            require(!eta.isNaN && 0d <= eta && eta <= 1d)
            if (j == numStepSizes) eta
            else {
                val etaNew = stepSizes.iapply(j)
                val objValNew = objVals(j)
                if (objValNew < this.objVal + pg * etaNew) {
                    etaNew
                } else {
                    rec(j + 1, etaNew)
                }
            }
        }.ensuring(res => 0d <= res && res <= 1d)

        rec(0, 0.0)
    }.ensuring(res => 0d <= res && res <= 1d) // partial specification, more annotations needed to show this
}

// Std dependencies

def indices(from: Int, to: Int): List[Int] = {
    require(0 <= from && from <= to)
    decreases(to - from)
    if from == to then
        Nil()
    else 
        forall_imply(indices(from + 1, to), x => x >= from + 1 && x < to, x => x >= from && x < to)
        Cons(from, indices(from + 1, to))
    
}.ensuring(res => 
    res.forall(x => x >= from && x < to)
)

def forall_imply[T](l: List[T], p1: T => Boolean, p2: T => Boolean): Unit = {
    require(forall((t: T) => p1(t) ==> p2(t)))
    l match {
        case Cons(h, t) => 
            forall_imply(t, p1, p2)
        case Nil() => ()
    }
}.ensuring(_ => l.forall(p1) ==> l.forall(p2))
        
def map_forall[T, U](l: List[T], f: T => U, p: U => Boolean): Unit = {
  require(forall((e: T) => p(f(e))))
  l match
    case Cons(h, t) => map_forall(t, f, p)
    case Nil() => ()
}.ensuring(l.map(f).forall(p))


