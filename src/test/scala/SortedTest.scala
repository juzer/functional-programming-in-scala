import org.scalatest.{FunSuite, Matchers}

class SortedTest extends FunSuite with Matchers {

  test("should return true on sorted int array") {
    assert(Sorted.isSorted(Array(9, 87, 156, 4331, 78732), (a: Int, b: Int) => a <= b) == true)
  }

  test("should return false on unsorted int array") {
    assert(Sorted.isSorted(Array(9, 87, 156, 4331, 409), (a: Int, b: Int) => a <= b) == false)
  }

  test("should return true on reverse sorted int array") {
    assert(Sorted.isSorted(Array(9543, 857, 156, 31, 6, -10), (a: Int, b: Int) => a >= b) == true)
  }

  test("should return true on sorted char array") {
    assert(Sorted.isSorted("abcdefgh".toCharArray, (a: Char, b: Char) => a <= b) == true)
  }

  test("should return false on unsorted char array") {
    assert(Sorted.isSorted("aBcdefgh".toCharArray, (a: Char, b: Char) => a <= b) == false)
  }

  test("should return true on sorted object array") {
    assert(Sorted.isSorted(Array(new Sample, new Sample, new Sample, new Sample), (a: Sample, b: Sample) => a.born <= b.born) == true)
  }

  class Sample {
    val born = System.currentTimeMillis()
  }
}
