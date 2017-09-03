import org.scalatest.{FunSuite, Matchers}

class FibTest extends FunSuite with Matchers {

  test("f(3) result should be 2") {
    assert(Fib.fib(3) == 2)
  }

  test("f(4) result should be 3") {
    assert(Fib.fib(4) == 3)
  }

  test("f(5) result should be 5") {
    assert(Fib.fib(5) == 5)
  }


}
