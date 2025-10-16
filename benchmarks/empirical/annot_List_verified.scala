package annot

// https://github.com/LIP17/fpinscala/blob/57e02da1a2a3ea4ca73548d52bee6fb99a90a88c/exercises/src/main/scala/fpinscala/datastructures/List.scala

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  private def productSpec(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * productSpec(xs)
  }

  // TO SPECIFY: 336
  def product(ds: List[Double]): Double = { ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }}.ensuring(_ == productSpec(ds))
}