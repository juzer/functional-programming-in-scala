package ch08

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class SumTest extends FunSuite with Checkers {

  override implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 0, maxSize = 0)

  test("sum of empty list = 0") {
    check {
      (l: List[Int]) =>
        l.sum == 0
    }
  }

}
