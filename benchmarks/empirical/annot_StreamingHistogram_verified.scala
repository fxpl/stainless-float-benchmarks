package annot_streaming_histogram_verified

import stainless.math.*
import stainless.lang.*
import stainless.collection.*
import stainless.annotation.*
import utils.Utils.*


//https://github.com/locationtech/geotrellis/blob/01a320ab4268308ef924d9fe3a8de09bd6de79cd/raster/src/main/scala/geotrellis/raster/histogram/StreamingHistogram.scala

/**
  * Ben-Haim, Yael, and Elad Tom-Tov. "A streaming parallel decision
  * tree algorithm."  The Journal of Machine Learning Research 11
  * (2010): 849-872.
  *
  * NOTE: The order in which values are counted could affect Bucket
  * distribution and counts as StreamingHistogram instances are merged,
  * due to the way in which bucket boundaries are defined.
  */

case class Bucket(label: Double, count: Long) {
  require(label.isFinite)
  require(count >= 0)

  def _1 = label
  def _2 = count
}

class StreamingHistogram(
  size: Int,
  minimum: Double = Double.PositiveInfinity,
  maximum: Double = Double.NegativeInfinity,
  buckets: List[Bucket] 
) {


  // TO SPECIFY: 16
  private def computeArea(a: Bucket, b: Bucket): Double = {
    val Bucket(value1, count1) = a
    val Bucket(value2, count2) = b
    val small: Long = if (count1 >= 0 && count2 >= 0) min(count1, count2); else max(count1, count2)
    val big: Long = if (count1 >= 0 && count2 >= 0) max(count1, count2); else min(count1, count2)
    val width: Double = abs(value1 - value2)
    if small == 0 && big == 0 then 0.0 else
    if width.isPosInfinity then Double.PositiveInfinity else
    (width * small) + (0.5 * width * (big - small))
  }.ensuring(res => res >= 0)

   /**
    * Return the area under the curve.
    */
  // TO SPECIFY: 17
  def areaUnderCurve(): Double = {
    computeAreaMap(buckets.sliding(2))
    buckets
      .sliding(2)
      .map({
        case Cons(x,Cons(y,Nil())) => computeArea(x,y)
        case _ => 0.0
      })
      .sum
  }.ensuring(res => res >= 0)

  @opaque
  def computeAreaMap(@induct l: List[List[Bucket]]): Unit = {
  }.ensuring(
    l.map({
      case Cons(x, Cons(y, Nil())) => computeArea(x, y)
      case _ => 0.0
    }).forall(e => !e.isNaN && e >= 0)
  )
    
}