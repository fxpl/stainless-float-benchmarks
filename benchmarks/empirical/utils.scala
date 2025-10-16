package utils

import stainless.lang.*
import stainless.math
import stainless.collection.*
import stainless.annotation.*

@library
object Utils {

  object System {
    @extern
    def currentTimeMillis(): Long = {
      ???.asInstanceOf[Long]
    }.ensuring(res => res >= 0)

    @extern
    def nanoTime: Long = {
      java.lang.System.nanoTime()
    }.ensuring(res => res >= 0)
  }

  def map_forall[T, U](l: List[T], f: T => U, pre: T => Boolean, post: U => Boolean): Unit = {
    require(l.forall(pre))
    require(forall((e: T) => pre(e) ==> post(f(e))))
    l match
      case Cons(h, t) => map_forall(t, f, pre, post)
      case Nil() => ()
  }.ensuring(l.map(f).forall(post))

  def map_forall[T, U](l: List[T], f: T => U, p: U => Boolean): Unit = {
    require(forall((e: T) => p(f(e))))
    l match
      case Cons(h, t) => map_forall(t, f, p)
      case Nil() => ()
  }.ensuring(l.map(f).forall(p))

  @opaque
  def icountForall[T](@induct l: List[T], p1: T => Boolean, p2: T => Boolean): Unit = {
    require(forall((t: T) => p1(t) ==> p2(t)))
  }.ensuring(l.icount(p1) <= l.icount(p2))

  @opaque
  def forallIapply[T](l: List[T], p: T => Boolean, idx: Int): Unit = {
    require(l.forall(p))
    require(0 <= idx && idx < l.isize)
    l match
      case Cons(h, t) if idx > 0 => forallIapply(t, p, idx - 1)
      case _ => ()
    
  }.ensuring(p(l.iapply(idx)))

  extension (l: List[Double]) 
    @opaque
    def sum: Double = {
      l match
        case Cons(h, t) => h + t.sum
        case Nil() => 0.0
    }.ensuring(res => 
      (l.forall(x => x.isFinite) ==> !res.isNaN)
      && (l.forall(x => !x.isNaN && x >= 0) ==> (!res.isNaN && res >= 0))
    )

  extension [T] (l: List[T])
    def icount(p: T => Boolean): Int = {
      decreases(l.length)
      l match {
        case Nil() => 0
        case Cons(h, t) =>
          val tailCount = t.icount(p)
          (if (p(h) && tailCount < Int.MaxValue) 1 else 0) + tailCount
      }
    }.ensuring(res => 0 <= res && res <= l.isize)

    def sliding(n: BigInt): List[List[T]] = {
      if (n <= 0) Nil()
      else {
        l match {
          case Nil() => Nil()
          case _ if l.length < n => Nil()
          case _ => l.take(n) :: l.tail.sliding(n)
        }
      }
    }

    
  @library @opaque
  def binarySearch[T](arr: Array[T], key: T): Int = {
    ???.asInstanceOf[Int] // Placeholder for actual implementation
  }.ensuring(
    res => res < arr.length && 
    ((res >= 0 && arr(res) == key) || res == -1) &&
    (old(arr) == arr)
  )

    @library @opaque
  def binarySearch[T](arr: List[T], key: T): Int = {
    ???.asInstanceOf[Int] // Placeholder for actual implementation
  }.ensuring(
    res => res < arr.isize && 
    ((res >= 0 && arr.iapply(res) == key) || res == -1) &&
    (old(arr) == arr)
  )

}
