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

  test("should construct list using foldr") {
    assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  test("should calculate length of list") {
    assert(List.length(List(1, 2, 3)) == 3)
  }

  test("should return length of empty list as 0") {
    assert(List.length(List()) == 0)
  }

  test("should sum all elements") {
    assert(List.sumL(List(1, 2, 4, 6, 3, 3)) == 19)
  }

  test("sum of empty list should be zero") {
    assert(List.sumL(List()) == 0)
  }

  test("should return product of all elements") {
    assert(List.productL(List(1.0, 2.0, 4.0, 6.0, 3.0, 3.0)) == 432.0)
  }

  test("product of empty list should be zero") {
    assert(List.sumL(List()) == 0)
  }

  test("should calculate length of list using foldL") {
    assert(List.lengthL(List(1, 2, 3)) == 3)
  }

  test("should return length of empty list as 0 using foldL") {
    assert(List.lengthL(List()) == 0)
  }

  test("should reverse list") {
    assert(List.reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

  test("tricky foldRight should allow implementing length") {
    assert(List.trickyFoldRight(List(1, 2, 3, 4, 5), 0)((_, acc) => 1 + acc) == 5)
  }

  test("should append to list using foldR") {
    assert(List.appendR(List(1, 2, 3, 4, 5), List(6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }

  test("should append to list using foldL") {
    assert(List.appendL(List(1, 2, 3, 4, 5), List(6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }

  test("should unzip list") {
    assert(List.unzip(List(List(1, 2), List(3, 4, 5), List(6))) == List(1, 2, 3, 4, 5, 6))
  }
}
