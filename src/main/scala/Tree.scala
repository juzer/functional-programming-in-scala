import scala.io.StdIn

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

  def swap(tree: Option[Tree[Int]], lvl: Int, swpAt: Int): Option[Tree[Int]] = tree match {
    case Some(Branch(v: Int, l, r)) => {
      if (v == -1) {
        return tree
      }
      if (lvl == swpAt) {
        Some(Branch(v, r, l))
      } else {
        Some(Branch(v, swap(l, lvl + 1, swpAt), swap(r, lvl + 1, swpAt)))
      }
    }
    case Some(Leaf(v)) => {
      Some(Leaf(v))
    }
    case None => {
      None
    }
  }

  def swapBranches(t: Branch[Int]): Tree[Int] = {
    Branch(t.value, t.right, t.left)
  }

  def readInput(): (Int, Array[String], Int, Array[Int]) = {
    val n = StdIn.readLine().toInt

    val input: Array[String] = Array.ofDim(n + 1)
    for (i <- 1 to n) {
      input(i) = StdIn.readLine()
    }
    val t = StdIn.readLine().toInt
    val ks: Array[Int] = Array.ofDim(t)
    for (i <- 0 to t - 1) {
      ks(i) = StdIn.readLine().toInt
    }
    (n, input, t, ks)
  }

  def main(args: Array[String]) {
    val (n, input, t, ks) = readInput()
    val tree = buildNode(1, n, input)
    //    val input: Array[String] = "5\n2 3\n-1 4\n-1 5\n-1 -1\n-1 -1\n1\n2".split("\n")
    //    val n = input(0).toInt
    //    val t = input(n + 1)
    //    val ks = input.splitAt(n + 2)._2.map(_.toInt)
    // TODO swap on k*2...
    ks.foldRight(tree)((k, t) => {
      val newT = swap(Some(t), 1, k)
      inorderPrint(newT)
      println()
      newT.get
    })
  }
}