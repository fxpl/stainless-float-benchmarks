package annot_fpslogger

import stainless.annotation.*
import stainless.lang.*
import utils.Utils.System

// https://github.com/Th3Falc0n/Terra-Infinita/blob/76aa3da2a185d1dc19b086aa3d771e7c22fea6a5/terra/src/main/scala/com/dafttech/terra/FPSLogger.scala

class FPSLogger(var lastTimeNano: Long,
                var avgTime: Float = 0,
                var avgFrames: Float = 0,
                var frameTime: Float = 0,
                var frameFrames: Float = 0) {

  require(0 <= lastTimeNano && lastTimeNano <= System.nanoTime)
  require(0 <= avgTime && avgTime.isFinite)
  require(0 <= avgFrames && avgFrames.isFinite)
  require(0 <= frameTime && frameTime.isFinite)
  require(0 <= frameFrames && frameFrames.isFinite)

  // TO SPECIFY: 13
  // verification loops forever
  def tick: Float = {
    val tDif: Long = System.nanoTime - lastTimeNano
    lastTimeNano = System.nanoTime
    val tDifF: Float = tDif / 1000000000f
    var avgSPF: Float = 0

    if (avgTime > 0) avgSPF = avgTime / avgFrames

    avgTime /= 1f + avgSPF
    avgFrames /= 1f + avgSPF

    avgTime += tDifF
    avgFrames += 1

    frameTime += tDifF
    frameFrames += 1

    if (frameTime > 0.5f) {
      frameTime = 0
      frameFrames = 0
    }

    tDifF
  }.ensuring(res => res.isFinite && res >= 0)

}