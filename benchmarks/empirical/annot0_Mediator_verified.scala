package annot0_verified

import stainless.lang.*

// https://github.com/saddle/saddle/blob/99bdb486361b9204df6a2dc45de86ce92803ede5/saddle-core/src/main/scala/org/saddle/stats/Mediator.scala

/**
 * Mediator is an auxiliary class for O(N log k) rolling median. It is inspired by
 * AShelly's C99 implementation, which is (c) 2011 ashelly.myopenid.com and licensed
 * under the MIT license: http://www.opensource.org/licenses/mit-license
 *
 * Reference:
 *   http://stackoverflow.com/questions/5527437/rolling-median-in-c-turlach-implementation
 */
class Mediator(
  winSz: Int, 
  data: Array[Double], 
  heap: Array[Int], 
  totCt: Int // # items in data
) {
  require(winSz > 0, "Window length must be > 0!")
  require(data.length == winSz)
  require(heap.length == winSz)
  require(0 <= totCt && totCt <= winSz)
  require(forall((i: Int) => (0 <= i && i < winSz) ==> (0 <= heap(i) && heap(i) < winSz)))
  require(forall((i: Int) => (0 <= i && i < winSz) ==> data(i).isFinite))


  // heap array contains indexes of data array giving a max-mid-min heap structure centered at hMid:
  //   index: [0           ...            hMid           ...      winSz-1]
  //   value: [... | child2 child1 | max] mid  [min | child1 child2 | ...]
  //
  // such that data(heap(max)) <= data(heap(hMid)) <= data(heap(min))
  //
  // also, we maintain invariants:
  //   (a) size(minheap) <= size(maxheap)
  //   (b) size(minheap) >= size(maxheap) - 1

  private val hMid = winSz / 2   // heap(hMid) = x s.t. data(x) holds mid (between max/min heaps)
  
  // TO SPECIFY: 12
  def median: Double = {
    val v = data(heap(hMid))
    if winSz == 1 then v / 2 + data(heap(0)) / 2.0 else
    if ((totCt & 1) == 0)
      v / 2 + data(heap(hMid-1)) / 2.0
    else
      v
  }.ensuring(res => res.isFinite)
}