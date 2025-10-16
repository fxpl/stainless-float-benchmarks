import stainless.lang.*

/**
 * Showcasing how to specify loop behavior in conjunction with floating-point functionality.
 */
object FPLoop {

  val MIN = 1.0
  val SMALL_MAX = 100.0
  val LARGE_MAX = 1.0e30

  /**
   * The method specification states that the result of the function should be at least as large as the threshold
   */
  /*@
    @ requires xf > MIN;
    @ ensures \result >= SMALL_MAX;
    @*/
  def fploop(xf: Double): Double = {
    require(!xf.isNaN && xf > MIN)
    var f = xf
    /**
     * The loop invariant and variant state that the value will always be larger than 1 and that it will increase
     * in each iteration, respectively.
     */
    /*@
      @ loop_invariant f > MIN;
      @ decreasing (SMALL_MAX - (f - 1.0));
      @*/
    (while (f < SMALL_MAX) {
      decreases(0)//SMALL_MAX - (f - 1.0))  // floating-point measures not supported currently
      f = f + 1.0
    }).invariant(
      !f.isNaN && f > MIN
    )
    f
  }.ensuring(_ >= SMALL_MAX)

  /**
   * A modified version of fploop in which The function and contract have been modified to use a larger threshold.
   */
  /*@
    @ requires xf > MIN;
    @ ensures \result >= LARGE_MAX;
    @*/
  def fploop2(xf: Double) = {
    require(!xf.isNaN && xf > MIN)
    var f = xf
    /*@
      @ loop_invariant f > MIN;
      @ decreasing (LARGE_MAX - (f - 1.0));
      @*/
    (while (f < LARGE_MAX) {
      decreases(0)//LARGE_MAX - (f - 1.0)) // floating-point measures not supported currently
      f = f + 1.0
    }).invariant(
      !f.isNaN && f > MIN
    )
    f
  }.ensuring(_ >= LARGE_MAX)

  /**
   * a hybrid of the previous two examples. The smaller bound is used in the algorithm itself, while the larger bound
   * is used only in the specification in the loop variant.
   */
  /*@
    @ requires xf > MIN;
    @ ensures \result >= SMALL_MAX;
    @*/
  def fploop3(xf: Double) = {
    require(!xf.isNaN && xf > MIN)
    var f = xf
    /*@
      @ loop_invariant f > MIN;
      @ decreasing (LARGE_MAX - (f - 1.0));
      @*/
    (while (f < SMALL_MAX) {
      decreases(0)//LARGE_MAX - (f - 1.0)) // floating-point measures not supported currently
      f = f + 1.0
    }).invariant(
      !f.isNaN && f > MIN
    )
    f
  }.ensuring(_ >= SMALL_MAX)
}
