package ch06

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i >= 0) (i, rng2)
    else if (i == Int.MinValue) (Int.MaxValue, rng2)
    else (i * -1, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    (Math.abs(i.toDouble / Int.MaxValue.toDouble), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val tpl = rng.nextInt
    val tpl2 = double(tpl._2)
    ((tpl._1, tpl2._1), tpl2._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val tpl = double(rng)
    val tpl2 = tpl._2.nextInt
    ((tpl._1, tpl2._1), tpl2._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val tpl1 = double(rng)
    val tpl2 = double(tpl1._2)
    val tpl3 = double(tpl2._2)
    ((tpl1._1, tpl2._1, tpl3._1), tpl3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (i, rnd2) = rng.nextInt
        go(count - 1, i :: acc, rnd2)
      }
    }

    go(count, List(), rng)
  }

  val doubleByMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng) => {
      val (a, _) = ra(rng)
      val (b, _) = rb(rng)
      (f(a, b), rng)
    }
  }

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def intsWithSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val(a, rnga) = f(rng)
      g(a)(rnga)
    }
  }

  def mapWithFM[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2withFM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}