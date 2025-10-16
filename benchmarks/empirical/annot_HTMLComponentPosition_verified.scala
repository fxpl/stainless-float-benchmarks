package annot

import stainless.lang.* 

// https://github.com/outr/youi/blob/6c81b1262569cdaba4e81681362d909022fe3a8e/ui/js/src/main/scala/io/youi/component/extras/HTMLComponentPosition.scala

class HTMLComponentPosition {

    // TO SPECIFY: 280
    private def real(d: Double): Double = {
        if (d.isInfinite || d.isNaN) {
        0.0
        } else {
        d
        }
    }.ensuring(res => res.isFinite)
}
