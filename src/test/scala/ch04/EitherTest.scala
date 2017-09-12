package ch04

import org.scalatest.{FunSuite, Matchers}

class EitherTest extends FunSuite with Matchers {

  test("should map successfully") {
    assert(Right(7).map(_ * 7) == Right(49))
  }

  test("should map error to the same error") {
    assert(Left("this is an error").map("error") == Left("this is an error"))
  }

  test("") {
    assert(true)
  }
}
