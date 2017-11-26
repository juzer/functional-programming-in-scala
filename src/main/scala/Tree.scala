object Solution {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

//  class Node(v: Int, l: Node, r: Node) {
//    var value: Int = v
//    var left: Node = l
//    var right: Node = r
//  }

  def buildNode(n: Int, maxN: Int): Tree[Int] = {
    if (n == maxN) Leaf(n)
    else Branch(n, buildNode(n+1, maxN), buildNode())
  }

  def main(args: Array[String]) {
    val input = "3\n2 3\n-1 -1\n-1 -1\n2\n1\n1".split("\n")
    val n = input(0).toInt
    val tree = buildNode(1, n)
  }
}