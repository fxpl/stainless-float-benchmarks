package annot

import stainless.lang.*
import stainless.math

// https://github.com/zyysoft/flink/blob/7470e44354b97a0320c35eccb80342affa654714/flink-table/flink-table-planner/src/main/scala/org/apache/flink/table/plan/cost/DataSetCost.scala#L28

class DataSetCost(val rowCount: Double, val cpu: Double, val io: Double) {
    require(!rowCount.isNaN && rowCount >= 0)
    require(!cpu.isNaN && cpu >= 0)
    require(!io.isNaN && io >= 0)

    // TO SPECIFY: 255
    def divideBy(that: DataSetCost): Double = {
        var d: Double = 1
        var n: Double = 0
        if ((this.rowCount != 0) && !this.rowCount.isInfinite &&
        (that.rowCount != 0) && !that.rowCount.isInfinite)
        {
            d *= this.rowCount / that.rowCount
            n += 1
        }
        if ((this.cpu != 0) && !this.cpu.isInfinite && (that.cpu != 0) && !that.cpu.isInfinite) {
            d *= this.cpu / that.cpu
            n += 1
        }
        if ((this.io != 0) && !this.io.isInfinite && (that.io != 0) && !that.io.isInfinite) {
            d *= this.io / that.io
            n += 1
        }
        if (n == 0) {
            1.0
        }
        else{
            math.pow(d, 1 / n)
        }
        
    }.ensuring(res => res >= 0)
}