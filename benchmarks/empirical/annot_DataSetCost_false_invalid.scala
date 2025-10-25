package annot

import stainless.lang.*
import stainless.math

// https://github.com/zyysoft/flink/blob/7470e44354b97a0320c35eccb80342affa654714/flink-table/flink-table-planner/src/main/scala/org/apache/flink/table/plan/cost/DataSetCost.scala#L28

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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