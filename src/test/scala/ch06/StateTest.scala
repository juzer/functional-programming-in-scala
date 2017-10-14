package ch06

import ch06.RNG.Simple
import org.scalatest.{FunSuite, Matchers}

class StateTest extends FunSuite with Matchers {

  test("should return non negative int") {
    assert(RNG.nonNegativeInt(Simple(42))._1 > 0)
  }

  test("should return a double") {
    val dbl = RNG.double(Simple(42))._1
    assert(0.0 <= dbl && dbl < 1.0)
  }

  test("should return an integer and a double") {
    val ((intgr, dbl), _) = RNG.intDouble(Simple(42))
    assert(intgr.isInstanceOf[Int])
    assert(dbl.isInstanceOf[Double])
    assert(isInRange(dbl))
  }

  test("should return a double and an integer") {
    val ((dbl, intgr), _) = RNG.doubleInt(Simple(42))
    assert(intgr.isInstanceOf[Int])
    assert(dbl.isInstanceOf[Double])
    assert(isInRange(dbl))
  }

  test("should return 3 doubles") {
    val ((dbl1, dbl2, dbl3), _) = RNG.double3(Simple(42))
    assert(dbl1.isInstanceOf[Double])
    assert(dbl2.isInstanceOf[Double])
    assert(dbl3.isInstanceOf[Double])
    assert(isInRange(dbl1))
  }

  test("should generate a list of integers") {
    val ints = RNG.ints(5)(Simple(42))._1
    assert(ints.length == 5)
    assert(ints.takeWhile(_.isInstanceOf[Int]) == ints)
  }

  test("should return a double by map") {
    val dbl = RNG.doubleByMap(Simple(42))._1
    assert(0.0 <= dbl && dbl < 1.0)
  }

  test("should return an integer and a double with map") {
    val ((intgr, dbl), _) = RNG.randIntDouble(Simple(42))
    assert(intgr.isInstanceOf[Int])
    assert(dbl.isInstanceOf[Double])
    assert(isInRange(dbl))
  }

  test("should return a double and an integer with map") {
    val ((dbl, intgr), _) = RNG.randDoubleInt(Simple(42))
    assert(intgr.isInstanceOf[Int])
    assert(dbl.isInstanceOf[Double])
    assert(isInRange(dbl))
  }

  test("should generate a list of integers with sequence") {
    val ints = RNG.intsWithSequence(5)
    val list = ints(Simple(42))._1
    assert(list.length == 5)
    assert(list.takeWhile(_.isInstanceOf[Int]) == list)
  }

  private def isInRange(dbl1: Double) = 0 <= dbl1 && dbl1 < 1
}
