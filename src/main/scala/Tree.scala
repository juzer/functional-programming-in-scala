object Solution {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) extends Tree[A]

  def buildNode(n: Int, maxN: Int, nodes: Array[String]): Tree[Int] = {
    if (n == maxN) Leaf(n)

    val values = nodes(n).split(" ")
    val left = values(0).toInt
    val right = values(1).toInt

    def nodeOrDie(value: Int): Option[Tree[Int]] = value match {
      case -1 => Option.empty
      case _ => Option.apply(buildNode(value, maxN, nodes))
    }

    Branch(n, nodeOrDie(left), nodeOrDie(right))
  }

  def inorderPrint(tree: Option[Tree[Int]]): Unit = tree match {
    case Some(Leaf(v)) => {
      print(v)
      print(" ")
    }
    case Some(Branch(v, left, right)) => {
      inorderPrint(left)
      print(v)
      print(" ")
      inorderPrint(right)
    }
    case _ => {}
  }

  def main(args: Array[String]) {
    val input : Array[String] = "3\n2 3\n-1 -1\n-1 -1\n2\n1\n1".split("\n")
    val n = input(0).toInt
    val tree = buildNode(1, n, input)
    inorderPrint(Option.apply(tree))
  }
}