package ch04

import org.scalatest.{FunSuite, Matchers}

class OptionTest extends FunSuite with Matchers {

  test("should map the option value") {
    assert(Some(7).map(_ % 2) == Some(1))
  }

  test("should get value from non-empty option") {
    assert(Some(8).getOrElse() == 8)
  }

  test("should get default value from empty option") {
    assert(None.getOrElse(17) == 17)
  }

  test("should flat map a value from non-empty option") {
    assert(Some(71).flatMap(a => Some(a % 2)) == Some(1))
  }

  test("should return original option when non-empty") {
    assert(Some(9).orElse(Some(90)) == Some(9))
  }

  test("should return default when option is empty") {
    assert(None.orElse(Some(90)) == Some(90))
  }

  test("should filter matching value") {
    assert(Some(7).filter(_ == 7) == Some(7))
  }

  test("should filter out non-matching value") {
    assert(Some(7).filter(_ == 18) == None)
  }

  test("should map two options") {
    assert(None.map2(Some(5), Some(9))(_ * _) == Some(45))
  }

  test("should convert optional sequence") {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  test("should return None if optional sequence contains None") {
    assert(Option.sequence(List(Some(1), Some(2), None)) == None)
  }

  test("should traverse a list") {
    assert(Option.traverse(List("1", "2", "3"))(a => Some(a.toInt)) == Some(List(1, 2, 3)))
  }

  test("should convert optional sequence is sense of traverse") {
    assert(Option.sequence2(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }
}
