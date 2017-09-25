package ch05

import org.scalatest.{FunSuite, Matchers}

class StreamTest extends FunSuite with Matchers {

  test("should convert stream to list") {
    assert(Stream("s", "c", "a", "l", "a").toList() == List("s", "c", "a", "l", "a"))
  }

  test("should convert empty stream to empty list") {
    assert(Stream.empty.toList() == List.empty)
  }

  test("should take first n elements") {
    assert(Stream(1, 2, 3, 4, 5).take(3).toList() == List(1, 2, 3))
  }

  test("should return empty stream on taking elements of empty stream") {
    assert(Stream().take(3) == Empty)
  }

  test("should drop first n elements") {
    assert(Stream(1, 2, 3, 4, 5).drop(3).toList() == List(4, 5))
  }

  test("should return empty stream on dropping elements of empty stream") {
    assert(Stream().drop(3) == Empty)
  }

  test("should take elements while condition is satisfied") {
    assert(Stream(4, 2, 8, 3, 5).takeWhile(_ % 2 == 0).toList() == List(4, 2, 8))
  }

  test("should return empty stream on taking while elements of empty stream") {
    assert(Stream().takeWhile(_ => true) == Empty)
  }
}
