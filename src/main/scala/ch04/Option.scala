package ch04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(xa => b.map(xb => f(xa, xb)))
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def loop[A](xa: List[Option[A]], acc: List[A]): Option[List[A]] = xa match {
      case Some(x) :: xaa => loop(xaa, x :: acc)
      case Nil => Some(acc.reverse)
      case _ => None
    }

    loop(a, List())
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case a :: as => map2(f(a), traverse(as)(f))(_ :: _)
    case Nil => Some(Nil)
  }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(b => b)
}

