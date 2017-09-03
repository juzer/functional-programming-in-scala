package ch03

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def replaceHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else list match {
      case Nil => list
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => list
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => append(List(x), init(xs))
  }
}
