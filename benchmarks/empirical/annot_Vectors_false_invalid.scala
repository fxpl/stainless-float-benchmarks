package annot1

import stainless.lang.*
import stainless.math
import stainless.annotation.*
import stainless.collection.*
import utils.Utils.*

// https://github.com/MrLight0809/spark/blob/0863ff039b1f579e40e6cd5f6dc48af582d4c1c4/mllib/src/main/scala/org/apache/spark/mllib/linalg/Vectors.scala

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

sealed trait Vector {

  def size: Int

  /**
   * Number of nonzero elements. This scans all active values and count nonzeros.
   */
  def numNonzeros: Int

  /**
   * Returns the ratio of number of zeros by total number of values.
   */
  // TO SPECIFY: 463
  private def sparsity(): Double = {
    require(1 <= size)
    require(0 <= numNonzeros && numNonzeros <= size)
    1.0 - numNonzeros.toDouble / size
  }.ensuring(res => 0d <= res && res <= 1d)
}


/**
 * Factory methods for [[org.apache.spark.mllib.linalg.Vector]].
 * We don't use the name `Vector` because Scala imports
 * `scala.collection.immutable.Vector` by default.
 */
object Vectors {


  /**
   * Returns the p-norm of this vector.
   * @param vector input vector.
   * @param p norm.
   * @return norm in L^p^ space.
   */
  // TO SPECIFY: 464
  def norm(vector: Vector, p: Double): Double = {
    require(!p.isNaN)
    require(p >= 1.0, "To compute the p-norm of the vector, we require that you specify a p>=1. " +
      s"You specified p=$p.")
    val values = vector match {
      case DenseVector(vs) => vs
      case SparseVector(n, ids, vs) => vs
    }
    val size = values.isize

    if (p == 1) {
      var sum = 0.0
      var i = 0
      (while (i < size) {
        decreases(size - i)
        sum += math.abs(values.iapply(i))
        i += 1
      }).invariant(
        0 <= i && i <= size
          && (sum.isNaN || 0d <= sum)
      )
      sum
    } else if (p == 2) {
      var sum = 0.0
      var i = 0
      (while (i < size) {
        decreases(size - i)
        sum += values.iapply(i) * values.iapply(i)
        i += 1
      }).invariant(
        0 <= i && i <= size
          && (sum.isNaN || 0d <= sum)
      )
      math.sqrt(sum)
    } else if (p == Double.PositiveInfinity) {
      var max = 0.0
      var i = 0
      (while (i < size) {
        decreases(size - i)
        val value = math.abs(values.iapply(i))
        if (!value.isNaN && value > max) max = value
        i += 1
      }).invariant(
        0 <= i && i <= size
          && !max.isNaN && 0d <= max
      )
      max
    } else {
      var sum = 0.0
      var i = 0
      (while (i < size) {
        decreases(size - i)
        sum += math.pow(math.abs(values.iapply(i)), p)
        i += 1
      }).invariant(
        0 <= i && i <= size
          && (sum.isNaN || 0d <= sum)
      )
      math.pow(sum, 1.0 / p)  // timeout since there is no lemma `pow(+, +) = +`
    }
  }.ensuring(res => res.isNaN || 0d <= res)

  // TO SPECIFY: 465
  def sqdist(v1: Vector, v2: Vector): Double = {
    require(v1.size == v2.size, s"Vector dimensions do not match: Dim(v1)=${v1.size} and Dim(v2)" +
      s"=${v2.size}.")
    var squaredDistance = 0.0
    (v1, v2) match {
      case (v1: SparseVector, v2: SparseVector) =>
        val v1Values = v1.values
        val v1Indices = v1.indices
        val v2Values = v2.values
        val v2Indices = v2.indices
        val nnzv1 = v1Indices.isize
        val nnzv2 = v2Indices.isize

        var kv1 = 0
        var kv2 = 0
        (while (kv1 < nnzv1 || kv2 < nnzv2) {
          decreases((nnzv2 - kv2, nnzv1 - kv1))
          var score = 0.0


          if (kv2 >= nnzv2 || (kv1 < nnzv1 && v1Indices.iapply(kv1) < v2Indices.iapply(kv2))) {
            score = v1Values.iapply(kv1)
            kv1 += 1
          } else if (kv1 >= nnzv1 || (kv2 < nnzv2 && v2Indices.iapply(kv2) < v1Indices.iapply(kv1))) {
            score = v2Values.iapply(kv2)
            kv2 += 1
          } else {
            score = v1Values.iapply(kv1) - v2Values.iapply(kv2)
            kv1 += 1
            kv2 += 1
          }
          squaredDistance += score * score
        }).invariant(
          0 <= kv1 && kv1 <= nnzv1
            && 0 <= kv2 && kv2 <= nnzv2
            && (squaredDistance.isNaN || 0d <= squaredDistance)
        )

      case (v1: SparseVector, v2: DenseVector) =>
        squaredDistance = sqdist(v1, v2)

      case (v1: DenseVector, v2: SparseVector) =>
        squaredDistance = sqdist(v2, v1)

      case (DenseVector(vv1), DenseVector(vv2)) =>
        var kv = 0
        val sz = vv1.isize
        (while (kv < sz) {
          decreases(sz - kv)
          val score = vv1.iapply(kv) - vv2.iapply(kv)
          squaredDistance += score * score
          kv += 1
        }).invariant(
          0 <= kv && kv <= sz
            && (squaredDistance.isNaN || 0d <= squaredDistance)
        )
    }
    squaredDistance
  }.ensuring(res => res.isNaN || 0d <= res)

  // new lemma for `sqdist` function below
  def applyForall[A](l: List[A], P: A => Boolean, i: Int): Unit = {
    decreases(l.length)
    require(0 <= i && i < l.isize)
    require(l.forall(P))
    l match {
      case Nil() => ()
      case Cons(x, xs) => if i == 0 then assert(P(x)) else applyForall(xs, P, i - 1)
    }
  }.ensuring(P(l.iapply(i)))

  /**
   * Returns the squared distance between DenseVector and SparseVector.
   */
  // TO SPECIFY: 466
  private def sqdist(v1: SparseVector, v2: DenseVector): Double = {
    var kv1 = 0
    var kv2 = 0
    val indices = v1.indices
    var squaredDistance = 0.0
    val nnzv1 = indices.isize
    val nnzv2 = v2.size
    var iv1 = if (nnzv1 > 0) indices.iapply(kv1) else -1

    (while (kv2 < nnzv2) {
      decreases(nnzv2 - kv2)
      var score = 0.0
      
      if (kv2 != iv1) {
        score = v2(kv2)
      } else {
        score = v1.values.iapply(kv1) - v2(kv2)
        if (kv1 < nnzv1 - 1) {
          kv1 += 1
          iv1 = indices.iapply(kv1)
          applyForall(indices, (i: Int) => 0 <= i && i < v1.values.isize, kv1)
        }
      }
      squaredDistance += score * score
      kv2 += 1
    }).invariant(
      0 <= kv2 && kv2 <= nnzv2
        && (nnzv1 == 0 && kv1 == 0 || 0 <= kv1 && kv1 < nnzv1)
        && (iv1 == -1 || 0 <= iv1 && iv1 < v1.values.isize)
        && (squaredDistance.isNaN || 0d <= squaredDistance)
    )
    squaredDistance
  }.ensuring(res => res.isNaN || 0d <= res)

 
  /** Max number of nonzero entries used in computing hash code. */
  val MAX_HASH_NNZ = 128


}

// /**
//  * A dense vector represented by a value array.
//  */
case class DenseVector (values: List[Double]) extends Vector {

  override def size: Int = values.isize


  def apply(i: Int): Double = {
    require(0 <= i && i < size) // Added
    values.iapply(i)
  }

  override def numNonzeros: Int = {
    values.icount(el => el.isNaN || el != 0.0)
  }

}

/**
 * A sparse vector represented by an index array and a value array.
 *
 * @param size size of the vector.
 * @param indices index array, assume to be strictly increasing.
 * @param values value array, must have the same length as the index array.
 */
case class SparseVector (
    size: Int,
    indices: List[Int],
    values: List[Double]) extends Vector {

  require(size >= 0, "The size of the requested sparse vector must be no less than 0.")
  require(indices.isize == values.isize, "Sparse vectors require that the dimension of the" +
    s" indices match the dimension of the values. You provided ${indices.length} indices and " +
    s" ${values.length} values.")
  require(indices.isize <= size, s"You provided ${indices.length} indices and values, " +
    s"which exceeds the specified vector size ${size}.")

  require(indices.forall((i : Int) => 0 <= i && i < values.isize))


  // TO SPECIFY: 468
  def apply(i: Int): Double = {
    require(i >= 0 && i < size, s"Index $i is out of bounds for vector of size $size.")

    val idx = binarySearch(indices, i)
    if (idx < 0) 0.0 else
      values.iapply(idx)
  }.ensuring(true)

  override def numNonzeros: Int = {
    values.icount(el => el.isNaN || el != 0.0)
  }
}


