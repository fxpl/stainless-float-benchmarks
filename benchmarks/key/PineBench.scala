import stainless.lang.*

object PineBench {

//  // valid
//  /*@ public normal_behaviour
//    @  requires 0.0f <= x0 && x0 <= 0.0f && 0.0f <= x1 && x1 <= 0.0f && 0.0f <= r && r <= 1.0f
//    @   && 0.0f <= x2 && x2 <= 0.0f && 0.0f <= x3 && x3 <= 0.0f  && -1.0f <= in0 && in0 <= 1.0f;
//    @  diverges true;
//    @*/
//  def ex2_reset_old(r: Float, _x0: Float, _x1: Float, _x2: Float, _x3: Float, in0: Float): Float = {
//    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 0.0f)
//    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 0.0f)
//    require(!r.isNaN && 0.0f <= r && r <= 1.0f)
//    require(!_x2.isNaN && 0.0f <= _x2 && _x2 <= 0.0f)
//    require(!_x3.isNaN && 0.0f <= _x3 && _x3 <= 0.0f)
//    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
//    var x0 = _x0
//    var x1 = _x1
//    var x2 = _x2
//    var x3 = _x3
//    /*@
//      @ loop_invariant -0.98f <= x0 && x0 <= 1.0f && -0.98f <= x1 && x1 <= 1.0f &&
//      @  -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
//      @  && 0.044f*x0 + -0.046f*x1 + -0.021f*x2 + 0.021f*x3 + 0.534f*x0*x0 + -1.0f*x0*x1 +
//      @  -0.53f*x0*x2 + 0.455f*x0*x3 + 0.531f*x1*x1 + 0.495f*x1*x2 + -0.49f*x1*x3 + 0.138f*x2*x2 +
//      @  -0.226f*x2*x3 + 0.118f*x3*x3 <= 0.043f;
//      @*/
//    (while (r == 0.0f) {
//      decreases(0) // original benchmark diverges
//      if (r > 0.5f) {
//        x0 = 1.5f * x0 - 0.7f * x1 - 0.7f * x2 + 0.4f * x3 + 0.5f * in0
//        x1 = x0
//        x2 = in0
//        x3 = x2
//      }
//      else {
//        x0 = 1.0f
//        x1 = 1.0f
//        x2 = 1.0f
//        x3 = 1.0f
//      }
//    }).invariant(
//      !x0.isNaN && !x1.isNaN && !x2.isNaN && !x3.isNaN
//        && -0.98f <= x0 && x0 <= 1.0f && -0.98f <= x1 && x1 <= 1.0f
//        && -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
//        && 0.044f*x0 + -0.046f*x1 + -0.021f*x2 + 0.021f*x3 + 0.534f*x0*x0 + -1.0f*x0*x1 +
//        -0.53f*x0*x2 + 0.455f*x0*x3 + 0.531f*x1*x1 + 0.495f*x1*x2 + -0.49f*x1*x3 + 0.138f*x2*x2 +
//        -0.226f*x2*x3 + 0.118f*x3*x3 <= 0.043f
//    )
//    r
//  }

  //valid
  /*@ public normal_behaviour
    @  requires 0.0f <= x0 && x0 <= 0.0f && 0.0f <= x1 && x1 <= 0.0f && 0.0f <= r && r <= 1.0f
    @   && 0.0f <= x2 && x2 <= 0.0f && 0.0f <= x3 && x3 <= 0.0f  && -1.0f <= in0 && in0 <= 1.0f;
    @  diverges true;
    @*/
  def ex2_reset(r: Float, _x0: Float, _x1: Float, _x2: Float, _x3: Float, in0: Float): Float = {
    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 0.0f)
    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 0.0f)
    require(!r.isNaN && 0.0f <= r && r <= 1.0f)
    require(!_x2.isNaN && 0.0f <= _x2 && _x2 <= 0.0f)
    require(!_x3.isNaN && 0.0f <= _x3 && _x3 <= 0.0f)
    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
    var x0 = _x0
    var x1 = _x1
    var x2 = _x2
    var x3 = _x3
    /*@
      @ loop_invariant -0.98f <= x0 && x0 <= 1.0f && -0.98f <= x1 && x1 <= 1.0f &&
      @  -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
      @  && 0.044f*x0 + -0.046f*x1 + -0.021f*x2 + 0.021f*x3 + 0.534f*x0*x0 + -1.0f*x0*x1 +
      @  -0.53f*x0*x2 + 0.455f*x0*x3 + 0.531f*x1*x1 + 0.495f*x1*x2 + -0.49f*x1*x3 + 0.138f*x2*x2 +
      @  -0.226f*x2*x3 + 0.118f*x3*x3 <= 0.043f;
      @*/
    (while (r == 0.0f) {
      decreases(0) // original benchmark diverges
      if (r > 0.5f) {
        val x01 = x0
        val x11 = x1
        val x21 = x2
        val x31 = x3
        x0 = 1.5f * x01 - 0.7f * x11 - 0.7f * x21 + 0.4f * x31 + 0.5f * in0
        x1 = x11
        x2 = in0
        x3 = x21
      }
      else {
        x0 = 1.0f
        x1 = 1.0f
        x2 = 1.0f
        x3 = 1.0f
      }
    }).invariant(
      !x0.isNaN && !x1.isNaN && !x2.isNaN && !x3.isNaN
        && -0.98f <= x0 && x0 <= 1.0f && -0.98f <= x1 && x1 <= 1.0f
        && -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
        && 0.044f*x0 + -0.046f*x1 + -0.021f*x2 + 0.021f*x3 + 0.534f*x0*x0 + -1.0f*x0*x1 +
      -0.53f*x0*x2 + 0.455f*x0*x3 + 0.531f*x1*x1 + 0.495f*x1*x2 + -0.49f*x1*x3 + 0.138f*x2*x2 +
      -0.226f*x2*x3 + 0.118f*x3*x3 <= 0.043f
    )
    r
  }

//  // CE
//  /*@ public normal_behaviour
//    @  requires 0.0f <= x0 && x0 <= 0.0f && 0.0f <= x1 && x1 <= 0.0f
//    @   && 0.0f <= x2 && x2 <= 0.0f && 0.0f <= x3 && x3 <= 0.0f  && -1.0f <= in0 && in0 <= 1.0f;
//    @  diverges true;
//    @*/
//  def ex2_old(_x0: Float, _x1: Float, _x2: Float, _x3: Float, in0: Float): Float = {
//    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 0.0f)
//    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 0.0f)
//    require(!_x2.isNaN && 0.0f <= _x2 && _x2 <= 0.0f)
//    require(!_x3.isNaN && 0.0f <= _x3 && _x3 <= 0.0f)
//    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
//    var x0 = _x0
//    var x1 = _x1
//    var x2 = _x2
//    var x3 = _x3
//    /*@
//      @ loop_invariant -1.0f <= x0 && x0 <= 1.05f && -1.0f <= x1 && x1 <= 1.05f &&
//      @  -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
//      @  && -0.001f*x1 + 0.534f*x0*x0 + -1.0f*x0*x1 + -0.532f*x0*x2 + 0.46f*x0*x3 + 0.527f*x1*x1
//      @  + 0.498f*x1*x2 + -0.493f*x1*x3 + 0.144f*x2*x2 + -0.23f*x2*x3 + 0.128f*x3*x3 <= 0.063f;
//      @*/
//    (while (x0 == 0.0f) {
//      decreases(0) // original benchmark diverges
//      x0 = 1.5f * x0 - 0.7f * x1 - 0.7f * x2 + 0.4f * x3 + 0.5f * in0
//      x1 = x0
//      x2 = in0
//      x3 = x2
//    }).invariant(
//      !x0.isNaN && !x1.isNaN && !x2.isNaN && !x3.isNaN
//        && -1.0f <= x0 && x0 <= 1.05f && -1.0f <= x1 && x1 <= 1.05f
//        && -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
//        && -0.001f*x1 + 0.534f*x0*x0 + -1.0f*x0*x1 + -0.532f*x0*x2 + 0.46f*x0*x3 + 0.527f*x1*x1
//        + 0.498f*x1*x2 + -0.493f*x1*x3 + 0.144f*x2*x2 + -0.23f*x2*x3 + 0.128f*x3*x3 <= 0.063f
//    )
//    x0
//  }

  // CE
  /*@ public normal_behaviour
    @  requires x0 == 0.0f && x1 == 0.0f
    @   && x2 == 0.0f && x3 == 0.0f  && -1.0f <= in0 && in0 <= 1.0f;
    @  diverges true;
    @*/
  def ex2(_x0: Float, _x1: Float, _x2: Float, _x3: Float, in0: Float): Float = {
    require(!_x0.isNaN && _x0 == 0.0f)
    require(!_x1.isNaN && _x1 == 0.0f)
    require(!_x2.isNaN && _x2 == 0.0f)
    require(!_x3.isNaN && _x3 == 0.0f)
    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
    var x0 = _x0
    var x1 = _x1
    var x2 = _x2
    var x3 = _x3
    /*@
      @ loop_invariant -1.0f <= x0 && x0 <= 1.05f && -1.0f <= x1 && x1 <= 1.05f &&
      @  -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
      @  && -0.001f*x1 + 0.534f*x0*x0 + -1.0f*x0*x1 + -0.532f*x0*x2 + 0.46f*x0*x3 + 0.527f*x1*x1 + 0.498f*x1*x2 + -0.493f*x1*x3 + 0.144f*x2*x2 + -0.23f*x2*x3 + 0.128f*x3*x3 <= 0.063f;
      @*/
    (while (x0 == 0.0f) {
      decreases(0) // original benchmark diverges
      val x01 = x0
      val x21 = x2
      val x11 = x1
      val x31 = x3
      x0 = 1.5f * x01 - 0.7f * x11 - 0.7f * x21 + 0.4f * x31 + 0.5f * in0
      x1 = x01
      x2 = in0
      x3 = x21
    }).invariant(
      !x0.isNaN && !x1.isNaN && !x2.isNaN && !x3.isNaN
        && -1.0f <= x0 && x0 <= 1.05f && -1.0f <= x1 && x1 <= 1.05f
        && -1.0f <= x2 && x2 <= 1.0f && -1.0f <= x3 && x3 <= 1.0f
        && -0.001f*x1 + 0.534f*x0*x0 + -1.0f*x0*x1 + -0.532f*x0*x2 + 0.46f*x0*x3 + 0.527f*x1*x1 + 0.498f*x1*x2 + -0.493f*x1*x3 + 0.144f*x2*x2 + -0.23f*x2*x3 + 0.128f*x3*x3 <= 0.063f
    )
    x0
  }

//  // valid
//  /*@ public normal_behaviour
//    @ requires 0.0f <= x0 && x0 <= 0.0f && 0.0f <= x1 && x1 <= 0.0f && 0.0f <= r && r <= 1.0f && -1.0f <= in0 && in0 <= 1.0f;
//    @ diverges true;
//    @*/
//  def ex3_reset_leadlag_old(_x0: Float, _x1: Float, in0: Float, r: Float): Float = {
//    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 0.0f)
//    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 0.0f)
//    require(!r.isNaN && 0.0f <= r && r <= 1.0f)
//    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
//    var x0 = _x0
//    var x1 = _x1
//    /*@
//      @ loop_invariant -3.0f <= x0 && x0 <= 3.0f && -5.0f <= x1 && x1 <= 6.0f
//      @  && -0.2f*x1 + 1.0f*x0*x0 + 0.3f*x0*x1 + 0.2f*x1*x1 <= 9.1f;
//      @*/
//    (while (x0 == 0.0f) {
//      decreases(0) // original benchmark diverges
//      if (r > 0.5f) {
//        x0 = 0.499f * x0 - 0.05f * x1 + in0
//        x1 = 0.01f * x0 + x1
//      }
//      else {
//        x0 = 1.0f
//        x1 = 1.0f
//      }
//    }).invariant(
//      !x0.isNaN && !x1.isNaN
//        && -3.0f <= x0 && x0 <= 3.0f && -5.0f <= x1 && x1 <= 6.0f
//        && -0.2f*x1 + 1.0f*x0*x0 + 0.3f*x0*x1 + 0.2f*x1*x1 <= 9.1f
//    )
//    x0
//  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= x0 && x0 <= 0.0f && 0.0f <= x1 && x1 <= 0.0f && 0.0f <= r && r <= 1.0f && -1.0f <= in0 && in0 <= 1.0f;
    @ diverges true;
    @*/
  def ex3_reset_leadlag(_x0: Float, _x1: Float, in0: Float, r: Float): Float = {
    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 0.0f)
    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 0.0f)
    require(!r.isNaN && 0.0f <= r && r <= 1.0f)
    require(!in0.isNaN && -1.0f <= in0 && in0 <= 1.0f)
    var x0 = _x0
    var x1 = _x1
    /*@
      @ loop_invariant -3.0f <= x0 && x0 <= 3.0f && -5.0f <= x1 && x1 <= 6.0f
      @  && -0.2f*x1 + 1.0f*x0*x0 + 0.3f*x0*x1 + 0.2f*x1*x1 <= 9.1f;
      @*/
    (while (x0 == 0.0f) {
      decreases(0) // original benchmark diverges
      val x01 = x0
      val x11 = x1
      if (r > 0.5f) {
        x0 = 0.499f * x01 - 0.05f * x11 + in0
        x1 = 0.01f * x01 + x11
      }
      else {
        x0 = 1.0f
        x1 = 1.0f
      }
    }).invariant(
      !x0.isNaN && !x1.isNaN
        && -3.0f <= x0 && x0 <= 3.0f && -5.0f <= x1 && x1 <= 6.0f
        && -0.2f*x1 + 1.0f*x0*x0 + 0.3f*x0*x1 + 0.2f*x1*x1 <= 9.1f
    )
    x0
  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= x0 && x0 <= 1.0f && 0.0f <= x1 && x1 <= 1.0f;
    @ diverges true;
    @*/
  def ex7_dampened(_x0: Float, _x1: Float): Float = {
    require(!_x0.isNaN && 0.0f <= _x0 && _x0 <= 1.0f)
    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 1.0f)
    var x0 = _x0
    var x1 = _x1
    /*@
      @ loop_invariant -1.0f <= x0 && x0 <= 1.4f && -3.9f <= x1 && x1 <= 3.1f
      @  && -0.38f*x0 + 0.05f*x1 + 1.0f*x0*x0 + 0.08f*x0*x1 + 0.11f*x1*x1 <= 1.33f;
      @*/
    (while (x0 == 0.0f) {
      decreases(0) // original benchmark diverges
      val x01 = x0
      val x11 = x1
      x0 = x01 + 0.01f * x11
      x1 = -0.1f * x01 + 0.99f * x11
    }).invariant(
      !x0.isNaN && !x1.isNaN
        && -1.0f <= x0 && x0 <= 1.4f && -3.9f <= x1 && x1 <= 3.1f
        && -0.38f*x0 + 0.05f*x1 + 1.0f*x0*x0 + 0.08f*x0*x1 + 0.11f*x1*x1 <= 1.33f
    )
    x0
  }

  // Valid // Valid
  /*@ public normal_behaviour
    @ requires 0.0f < x && x < 1.0f && 0.0f < y && y < 1.0f;
    @ diverges true;
    @*/
  def filter_goubault(_x: Float, _y: Float): Float = {
    require(!_x.isNaN && 0.0f < _x && _x < 1.0f)
    require(!_y.isNaN && 0.0f < _y && _y < 1.0f)
    var x = _x
    var y = _y
    //-0.483*x + -0.501*y + 0.987*x^2 + -0.492*x*y + 1.0*y^2 <= 0.511
    /*@
      @ loop_invariant -0.34f <= x && x <= 1.0f && -0.34f <= y && y <= 1.0f
      @  && -0.483f*x + -0.501f*y + 0.987f*x*x + -0.492f*x*y + 1.0f*y*y <= 0.511f;
      @*/
    (while (x == 0.0f) {
      decreases(0) // original benchmark diverges
      val x1 = x
      val y1 = y
      x = 0.75f * x1 - 0.125f * y1
      y = x1
    }).invariant(
      !x.isNaN && !y.isNaN
        && -0.34f <= x && x <= 1.0f && -0.34f <= y && y <= 1.0f
        && -0.483f*x + -0.501f*y + 0.987f*x*x + -0.492f*x*y + 1.0f*y*y <= 0.511f
    )
    x
  }

  // Valid
  /*@ public normal_behaviour
  @ requires -1.0f <= x && x <= 1.0f && -1.0f <= y && y <= 1.0f;
  @ diverges true;
  @*/
  def filterMine1(_x: Float, _y: Float): Float = {
    require(!_x.isNaN && -1.0f <= _x && _x <= 1.0f)
    require(!_y.isNaN && -1.0f <= _y && _y <= 1.0f)
    var x = _x
    var y = _y
    /*@
      @ loop_invariant -1.4f <= x && x <= 1.4f && -1.4f <= y && y <= 1.4f
      @  && -0.01f*x + -0.01f*y + 1.0f*x*x + -0.02f*x*y + 0.98f*y*y <= 2.0f;
      @*/
    (while (x == 0.0f) {
      decreases(0) // original benchmark diverges
      val x1 = x
      val y1 = y
      x = 0.68f * (x1 - y1)
      y = 0.68f * (x1 + y1)
    }).invariant(
      !x.isNaN && !y.isNaN
        && -1.4f <= x && x <= 1.4f && -1.4f <= y && y <= 1.4f
        && -0.01f*x + -0.01f*y + 1.0f*x*x + -0.02f*x*y + 0.98f*y*y <= 2.0f
    )
    x
  }

  // valid
  /*@ public normal_behaviour
    @ requires -0.1f <= s0 && s0 <= 0.1f && -0.1f <= s1 && s1 <= 0.1f && -0.1f <= n && n <= 0.1f;
    @ diverges true;
    @*/
  def filterMine2NonDet(_s0: Float, _s1: Float, n: Float): Float = {
    require(!_s0.isNaN && -0.1f <= _s0 && _s0 <= 0.1f)
    require(!_s1.isNaN && -0.1f <= _s1 && _s1 <= 0.1f)
    require(!n.isNaN && -0.1f <= n && n <= 0.1f)
    var s0 = _s0
    var s1 = _s1
    /*@
      @ loop_invariant -1.7f <= s1 && s1 <= 1.5f && -1.7f <= s0 && s0 <= 1.5f
      @ && (0.02f*s1) + (0.02f*s0) + (0.52f*s1*s1) + (-1.0f*s1*s0) + (0.59f*s0*s0) <= 0.29f;
      @*/
    (while (s0 == 0.0f) {
      decreases(0) // original benchmark diverges
      val s11 = s1
      val s01 = s0
      s1 = s01
      s0 = 1.5f * s01 - 0.7f * s11 + n
    }).invariant(
      !s0.isNaN && !s1.isNaN
        && -1.7f <= s1 && s1 <= 1.5f && -1.7f <= s0 && s0 <= 1.5f
        && (0.02f*s1) + (0.02f*s0) + (0.52f*s1*s1) + (-1.0f*s1*s0) + (0.59f*s0*s0) <= 0.29f
    )
    s0
  }

  // valid
  /*@ public normal_behaviour
    @ requires -0.1f <= s0 && s0 <= 0.1f && -0.1f <= s1 && s1 <= 0.1f;
    @ diverges true;
    @*/
  def filterMine2(_s0: Float, _s1: Float): Float = {
    require(!_s0.isNaN && -0.1f <= _s0 && _s0 <= 0.1f)
    require(!_s1.isNaN && -0.1f <= _s1 && _s1 <= 0.1f)
    var s0 = _s0
    var s1 = _s1
    /*@
      @ loop_invariant -0.35f <= s0 && s0 <= 0.37f && -0.35f <= s1 && s1 <= 0.37f
      @ && (-0.007f*s0) + (0.004f*s1) + (0.607f*s0*s0) + (-1.0f*s0*s1) + (0.569f*s1*s1) <= 0.023f;
      @*/
    (while (s0 == 0.0f) {
      decreases(0) // original benchmark diverges
      val s01 = s0
      val s11 = s1
      s0 = 1.5f * s01 - 0.7f * s11
      s1 = s01
    }).invariant(
      !s0.isNaN && !s1.isNaN
        && -0.35f <= s0 && s0 <= 0.37f && -0.35f <= s1 && s1 <= 0.37f
        && (-0.007f*s0) + (0.004f*s1) + (0.607f*s0*s0) + (-1.0f*s0*s1) + (0.569f*s1*s1) <= 0.023f
    )
    s0
  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= x1 && x1 <= 1.0f && 0.0f <= x2 && x2 <= 1.0f;
    @ diverges true;
    @*/
  def harmonic(_x1: Float, _x2: Float): Float = {
    require(!_x1.isNaN && 0.0f <= _x1 && _x1 <= 1.0f)
    require(!_x2.isNaN && 0.0f <= _x2 && _x2 <= 1.0f)
    var x1 = _x1
    var x2 = _x2
    /*@
      @ loop_invariant -0.4f <= x1 && x1 <= 1.7f  && -1.0f <= x2 && x2 <= 1.0f &&
      @ (-1.0f*x1) + (-0.41f*x2) + (0.82f*x1*x1) + (0.34f*x1*x2) + (0.74f*x2*x2) <= 0.52f;
      @*/
    (while (x1 == 0.0f) {
      decreases(0) // original benchmark diverges
      val x11 = x1
      val x21 = x2
      x1 = x11 + 0.01f * x21
      x2 = -0.01f * x11 + 0.99f * x21
    }).invariant(
      !x1.isNaN && !x2.isNaN
        && -0.4f <= x1 && x1 <= 1.7f  && -1.0f <= x2 && x2 <= 1.0f
        && (-1.0f*x1) + (-0.41f*x2) + (0.82f*x1*x1) + (0.34f*x1*x2) + (0.74f*x2*x2) <= 0.52f
    )
    x1
  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= u && u <= 0.0f && 2.0f <= v && v <= 3.0f;
    @ diverges true;
    @*/
  def PendulumSinAprox(_u: Float, _v: Float): Float = {
    require(!_u.isNaN && 0.0f <= _u && _u <= 0.0f)
    require(!_v.isNaN && 2.0f <= _v && _v <= 3.0f)
    var u = _u
    var v = _v
    /*@
      @ loop_invariant -1.1f <= u && u <= 1.2f  && -3.2f <= v && v <= 3.1f &&
      @ (-0.11f*u) + (0.01f*v) + (1.0f*u*u) + (0.03f*u*v) + (0.12f*v*v) <= 1.15f;
      @*/
    (while (u == 0.0f) {
      decreases(0) // original benchmark diverges
      val u1 = u
      val v1 = v
      u = u1 + 0.01f * v1
      v = v1 + 0.01f * (-0.5f * v1 - 9.81f * (u1 - (u1 * u1 * u1) / 6.0f + (u1 * u1 * u1 * u1 * u1) / 120.0f))
    }).invariant(
      !u.isNaN && !v.isNaN
        && -1.1f <= u && u <= 1.2f  && -3.2f <= v && v <= 3.1f
        && (-0.11f*u) + (0.01f*v) + (1.0f*u*u) + (0.03f*u*v) + (0.12f*v*v) <= 1.15f
    )
    u
  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= u && u <= 0.0f && 2.0f <= v && v <= 3.0f;
    @ diverges true;
    @*/
  def Pendulum_small(_u: Float, _v: Float): Float = {
    require(!_u.isNaN && 0.0f <= _u && _u <= 0.0f)
    require(!_v.isNaN && 2.0f <= _v && _v <= 3.0f)
    var u = _u
    var v = _v
    /*@
      @ loop_invariant -2.3f <= u && u <= 2.4f  && -7.2f <= v && v <= 6.6f &&
      @ -0.12f*u + 0.06f*v + 1.0f*u*u + 0.06f*u*v + 0.11f*v*v <= 5.16f;
      @*/
    (while (u == 0.0f) {
      decreases(0) // original benchmark diverges
      val u1 = u
      val v1 = v
      u = u1 + 0.01f * v1
      v = v1 + 0.01f * (-0.5f * v1 - 9.81f * u1)
    }).invariant(
      !u.isNaN && !v.isNaN
        && -2.3f <= u && u <= 2.4f  && -7.2f <= v && v <= 6.6f
        && -0.12f*u + 0.06f*v + 1.0f*u*u + 0.06f*u*v + 0.11f*v*v <= 5.16f
    )
    u
  }

  // valid
  /*@ public normal_behaviour
    @ requires 0.0f <= x && x <= 1.0f && 0.0f <= v && v <= 1.0f;
    @ diverges true;
    @*/
  def symplectic(_x: Float, _v: Float): Float = {
    require(!_x.isNaN && 0.0f <= _x && _x <= 1.0f)
    require(!_v.isNaN && 0.0f <= _v && _v <= 1.0f)
    var x = _x
    var v = _v
    /*@
      @ loop_invariant -0.4f <= x && x <= 1.3f  && -0.7f <= v && v <= 1.0f &&
      @ -0.73f*x + -0.35f*v + 1.0f*x*x + -0.27f*x*v + 0.82f*v*v <= 0.51f;
      @*/
    (while (x == 0.0f) {
      decreases(0) // original benchmark diverges
      val x1 = x
      val v1 = v
      x = 0.95f * x1 + 0.09975f * v1
      v = -0.1f * x1 + 0.95f * v1
    }).invariant(
      !x.isNaN && !v.isNaN
        && -0.4f <= x && x <= 1.3f  && -0.7f <= v && v <= 1.0f
        && -0.73f*x + -0.35f*v + 1.0f*x*x + -0.27f*x*v + 0.82f*v*v <= 0.51f
    )
    x
  }
}