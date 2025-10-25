package annot

import stainless.lang.*

// https://github.com/seL4/isabelle/blob/55b8c844db5feb0b7119e6d87a47b419f74234ed/src/Pure/GUI/gui.scala
/*  Title:      Pure/GUI/gui.scala
    Author:     Makarius

Basic GUI tools (for AWT/Swing).
*/

class Zoom(val percent: Int) {
    require(0 < percent)
    
    // TO SPECIFY: 366
    def scale: Double = {
        0.01 * percent
    }.ensuring(res => 0 < res && res <= Int.MaxValue * 0.01)
}