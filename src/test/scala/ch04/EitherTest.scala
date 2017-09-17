package ch04

import org.scalatest.{FunSuite, Matchers}

class EitherTest extends FunSuite with Matchers {

  test("should map successfully") {
    assert(Right(7).map(_ * 7) == Right(49))
  }

  test("should map error to the same error") {
    assert(Left("this is an error").map("error") == Left("this is an error"))
  }

  test("should flat map successfully") {
    assert(Right(7).flatMap(a => Right(a * 7)) == Right(49))
  }

  test("should flat map error to the same error") {
    assert(Left("this is an error").flatMap(e => Left(e)) == Left("this is an error"))
  }

  test("should get original value") {
    assert(Right(43).orElse(Right(-9)) == Right(43))
  }

  test("should get alternative value") {
    assert(Left(43).orElse(Right(-9)) == Right(-9))
  }

  test("should map2 successfully") {
    assert(Right(2).map2(Right(3))(_ + _) == Right(5))
  }

  test("should return left when object is left") {
    assert(Left("err").map2(Right(3))((a, _) => a) == Left("err"))
  }

  test("should return left when argument is left") {
    assert(Right(5).map2(Left("err"))((a, _) => a) == Left("err"))
  }

  test("should traverse a list") {
    assert(Either.traverse(List(1, 2, 3))(a => Right(a)) == Right(List(1, 2, 3)))
  }

  test("should sequence a list") {
    assert(Either.sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
  }
}
