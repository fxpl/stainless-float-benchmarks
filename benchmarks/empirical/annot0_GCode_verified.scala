package annot0_verified

import stainless.lang.*

// https://github.com/alexsmirnov/printrbot-g2-console/blob/a80fb48f22ebd770fb5c04306f277488c0d4e16b/src/main/scala/alexsmirnov/pbconsole/gcode/GCode.scala

object GCode {

    // TO SPECIFY: 54
    def dist(from: Option[Float], to: Option[Float]): Float = {
        require(from.isDefined ==> !from.get.isNaN)
        require(to.isDefined ==> !to.get.isNaN)
        from.flatMap { f => to.map( t =>
            if t.isInfinite && f.isInfinite && t == f then 0.0f else t - f
        ) }.getOrElse(0.0f)
    }.ensuring(res => !res.isNaN)

}