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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, acc) => 1 + acc)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumL(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productL(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def lengthL[A](list: List[A]): Int = foldLeft(list, 0)((acc, _) => 1 + acc)

  def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((acc, a) => Cons(a, acc))

  def trickyFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b, a) => f(a, b))

  def appendR[A](listA: List[A], listB: List[A]): List[A] = foldRight(listA, listB)((a, b) => Cons(a, b))

  def appendL[A](listA: List[A], listB: List[A]): List[A] = foldLeft(reverse(listA), listB)((a, b) => Cons(b, a))

  def unzip[A](list: List[List[A]]): List[A] = foldLeft(list, List[A]())((a: List[A], b: List[A]) => append(a, b))

  def incr(list: List[Int]) = reverse(foldLeft(list, List[Int]())((a, b) => Cons(b + 1, a)))

  def stringify(list: List[Double]) = reverse(trickyFoldRight(list, List[String]())((a, b) => Cons(a.toString(), b)))

  def map[A, B](as: List[A])(f: A => B): List[B] = reverse(foldLeft(as, List[B]())((a, b) => Cons(f(b), a)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    reverse(foldLeft(as, List[A]())((a, b) => if (f(b)) a else Cons(b, a)))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    reverse(foldLeft(as, List[B]())((a, b) => append(f(b), a)))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def add(listA: List[Int], listB: List[Int]): List[Int] = (listA, listB) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
  }

  def zipWith[A](listA: List[A], listB: List[A])(f: (A, A) => A): List[A] = (listA, listB) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }
}
