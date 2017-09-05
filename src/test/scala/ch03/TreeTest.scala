package ch03

import org.scalatest.{FunSuite, Matchers}

class TreeTest extends FunSuite with Matchers{

  //       N
  //     /   \
  //    /     \
  //   N       N
  //  / \     / \
  // 1   2   N   5
  //        / \
  //       3   4
  val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

  test("should calculate tree size") {
    assert(Tree.size(tree) == 9)
  }

  test("should return max element") {
    assert(Tree.max(tree) == 5)
  }

  test("should calculate tree depth") {
    assert(Tree.depth(tree) == 3)
  }

  test("should map all tree elements") {
    assert(Tree.map(tree)(_ * 3) == Branch(Branch(Leaf(3), Leaf(6)), Branch(Branch(Leaf(9), Leaf(12)), Leaf(15))))
  }

  test("should calculate tree size with fold") {
    assert(Tree.sizeFold(tree) == 9)
  }

  test("should return max element with fold") {
    assert(Tree.maxFold(tree) == 5)
  }

  test("should calculate tree depth with fold") {
    assert(Tree.depthFold(tree) == 3)
  }

  test("should map all tree elements with fold") {
    assert(Tree.mapFold(tree)(_ * 3) == Branch(Branch(Leaf(3), Leaf(6)), Branch(Branch(Leaf(9), Leaf(12)), Leaf(15))))
  }

}
