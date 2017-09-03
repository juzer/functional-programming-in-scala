package ch03

import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {

  test("should return tail of a list") {
    assert(List.tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  test("should return Nil as a tail of empty list") {
    assert(List.tail(List()) == Nil)
  }

  test("should replace head in a list") {
    assert(List.replaceHead(8, List(1, 2, 3, 4)) == List(8, 2, 3, 4))
  }

  test("should return Nil when replacing head of empty list") {
    assert(List.replaceHead(8, List()) == Nil)
  }

  test("should drop n first elements") {
    assert(List.drop(List(1, 2, 3, 4, 5, 6, 7), 5) == List(6, 7))
  }

  test("should return Nil when dropping elements of empty list") {
    assert(List.drop(List(), 7) == Nil)
  }

  test("should drop all matching elements") {
    assert(List.dropWhile(List(1, 1, 1, 1, 1, 6, 7))((x) => x == 1) == List(6, 7))
  }

  test("should return Nil when while dropping elements of empty list") {
    assert(List.dropWhile(List())((x) => x == 1) == Nil)
  }

  test("should return start of the list") {
    assert(List.init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  test("should return Nil as a start of an empty list") {
    assert(List.init(List()) == Nil)
  }

}
