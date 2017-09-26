package ch05

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //  @annotation.tailrec
  def toList(): List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) if n <= 0 => Cons(h, t)
    case Empty => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => false
    case _ => foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => {
    if (p(a)) cons(a, b)
    else Empty
  })

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => {
    if (f(a)) cons(a, b)
    else b
  })

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def mapUF[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeUF(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
    case _ => None
  }

  def takeWhileUF(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipWith(s)((a, b) => (a, b)).forAll { case (a, b) => a equals b }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some((cons(h(), t()), t()))
    case _ => None
  } append Stream(Empty)

//  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = unfold(this) {
//    case Cons(h, t) => Some((f(h(), t()), t()))
//    case _ => None
//  } append Stream(z)
//
//  def tailsScan: Stream[Stream[A]] = scanRight(empty[A])((a, b) => cons(a, b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fib(a1: Int, a2: Int): Stream[Int] = {
      cons(a1, fib(a2, a1 + a2))
    }

    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(p: Option[(A, S)]): Stream[A] = p match {
      case Some((a, s)) => cons(a, loop(f(s)))
      case None => Empty
    }

    loop(f(z))
  }

  def onesUF(): Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantUF[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromUF(a: Int): Stream[Int] = unfold(a)(a => Some(a, a + 1))

  def fibsUF(): Stream[Int] = unfold(0, 1) { case (a, b) => Some(a, (a + b, a)) }

}