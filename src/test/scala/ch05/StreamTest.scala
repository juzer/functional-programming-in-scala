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

  test("should return true if all elements conform predicate") {
    assert(Stream(4, 2, 8, 12, 4).forAll(_ % 2 == 0) == true)
  }

  test("should return false if any of the elements does not conform predicate") {
    assert(Stream(4, 2, 8, 7, 4).forAll(_ % 2 == 0) == false)
  }

  test("should return false when forAll is called on empty stream") {
    assert(Stream().forAll(_ => true) == false)
  }

  test("should take elements while condition is satisfied (Fold Right)") {
    assert(Stream(4, 2, 8, 3, 5).takeWhileFR(_ % 2 == 0).toList() == List(4, 2, 8))
  }

  test("should return empty stream on taking while elements of empty stream (Fold Right)") {
    assert(Stream().takeWhileFR(_ => true) == Empty)
  }

  test("should return non-empty optional on non-empty stream") {
    assert(Stream(1, 2, 3).headOption == Option(1))
  }

  test("should return empty optional on empty stream") {
    assert(Stream().headOption == Option.empty)
  }

  test("should map stream") {
    assert(Stream(1, 2, 3, 4).map(_ * 2).toList() == List(2, 4, 6, 8))
  }

  test("should filter stream") {
    assert(Stream(3, 2, 4, 5, 6).filter(_ % 2 == 0).toList() == List(2, 4, 6))
  }

  test("should append stream to stream") {
    assert(Stream(1, 2, 3, 4).append(Stream(5)).toList() == List(1, 2, 3, 4, 5))
  }

  test("should flat map stream") {
    assert(Stream(Stream(1, 2, 3), Stream(4), Stream(5, 6)).flatMap(a => a.map(_ * 2)).toList() == List(2, 4, 6, 8, 10, 12))
  }

  test("should create constant stream") {
    assert(Stream.constant("g").take(3).toList() == List("g", "g", "g"))
  }

  test("should generate consecutive integers") {
    assert(Stream.from(5).take(5).toList() == List(5, 6, 7, 8, 9))
  }

  test("should generate fibonacci sequence") {
    assert(Stream.fibs().take(7).toList() == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("should generate ones with unfold") {
    assert(Stream.onesUF().take(5).toList() == List(1, 1, 1, 1, 1))
  }

  test("should generate constant with unfold") {
    assert(Stream.constantUF("a").take(5).toList() == List("a", "a", "a", "a", "a"))
  }

  test("should generate consecutive integers with unfold") {
    assert(Stream.fromUF(5).take(5).toList() == List(5, 6, 7, 8, 9))
  }

  test("should generate fibonacci sequence with unfold") {
    assert(Stream.fibsUF().take(7).toList() == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("should map stream with unfold") {
    assert(Stream(1, 2, 3, 4).mapUF(_ * 2).toList() == List(2, 4, 6, 8))
  }

  test("should take first n elements with unfold") {
    assert(Stream(1, 2, 3, 4, 5).takeUF(3).toList() == List(1, 2, 3))
  }

  test("should return empty stream on taking elements of empty stream with unfold") {
    assert(Stream().takeUF(3) == Empty)
  }

  test("should take elements while condition is satisfied with unfold") {
    assert(Stream(4, 2, 8, 3, 5).takeWhileUF(_ % 2 == 0).toList() == List(4, 2, 8))
  }

  test("should return empty stream on taking while elements of empty stream with unfold") {
    assert(Stream().takeWhileUF(_ => true) == Empty)
  }

  test("should zip two streams") {
    assert(Stream(4, 2, 8, 3, 5).zipWith(Stream(3, 3, 3))((_, _)).toList() == List((4, 3), (2, 3), (8, 3)))
  }

  test("should zip all from two streams") {
    assert(Stream(4, 2, 8, 3, 5).zipAll(Stream("a", "b", "c")).toList() == List((Some(4), Some("a")), (Some(2), Some("b")), (Some(8), Some("c")), (Some(3), None), (Some(5), None)))
  }

  test("should return true if stream starts with given stream") {
    assert(Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)) == true)
  }

  test("should return false if stream does not start with given stream") {
    assert(Stream(1, 2, 3, 4, 5).startsWith(Stream(2, 3)) == false)
  }

  test("should return tails") {
    assert(Stream(1, 2, 3).tails.map(_.toList()).toList() == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

}
