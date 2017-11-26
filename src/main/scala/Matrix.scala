import scala.annotation.tailrec
import scala.io.StdIn

object Solution {

  type Layer = List[(Int, Int)]

  def Layer() = List[(Int, Int)]()

  type Layers = List[Layer]

  def Layers() = List[Layer]()


  def loadArray(rows: Int, cols: Int): Array[Array[Int]] = {
    val arr = Array.ofDim[Int](rows, cols)
    for (i <- 0 to rows - 1) {
      arr(i) = StdIn.readLine().split(" ").map(_.toInt)
      //      arr(i) = List.range(1, cols + 1).map(_ + (i * cols)).mkString(" ").split(" ").map(_.toInt)
    }
    //    val data = "9718805 60013003 5103628 85388216 21884498 38021292 73470430 31785927\n69999937 71783860 10329789 96382322 71055337 30247265 96087879 93754371\n79943507 75398396 38446081 34699742 1408833 51189 17741775 53195748\n79354991 26629304 86523163 67042516 54688734 54630910 6967117 90198864\n84146680 27762534 6331115 5932542 29446517 15654690 92837327 91644840\n58623600 69622764 2218936 58592832 49558405 17112485 38615864 32720798\n49469904 5270000 32589026 56425665 23544383 90502426 63729346 35319547\n20888810 97945481 85669747 88915819 96642353 42430633 47265349 89653362\n55349226 10844931 25289229 90786953 22590518 54702481 71197978 50410021\n9392211 31297360 27353496 56239301 7071172 61983443 86544343 43779176"
    //    val listOfRows = data.split("\n")
    //    listOfRows.map(_.split(" ").map(_.toInt))
    arr
  }

  def printArray(array: Array[Array[Int]]): Unit = {
    array.foreach(arr => {
      arr.foreach(i => print("%d ".format(i)))
      print("\n")
    })
  }

  def rollList[T](theList: List[T], itemsToRoll: Int): List[T] = {
    val split = {
      if (itemsToRoll < 0) (theList.size + itemsToRoll % theList.size)
      else if (theList.size == 0) 0
      else itemsToRoll % theList.size
    }
    val (beginning, end) = theList.splitAt(split)
    end ::: beginning
  }

  def peel(array: Array[Array[Int]], level: Int, rows: Int, cols: Int, list: Layers): Layers = level match {
    case _ if ((level < rows / 2) && (level < cols / 2)) => {
      var layer = Layer()
      for (j <- level to (cols - level - 1)) {
        layer = layer.+:((level, j))
      }
      for (i <- (level + 1) to (rows - level - 1)) {
        layer = layer.+:((i, cols - level - 1))
      }
      for (j <- level to (cols - level - 2)) {
        layer = layer.+:((rows - level - 1, cols - j - 2))
      }
      for (i <- level to (rows - level - 3)) {
        layer = layer.+:((rows - i - 2, level))
      }
      peel(array, level + 1, rows, cols, list.::(layer.reverse))
    }
    case _ if ((level == rows / 2) && (level == cols / 2) && (level % 2 == 1)) => {
      var layer = Layer()
      list.::(layer.::((level, level)))
    }
    case _ if ((level == rows / 2) && (level == cols / 2)) => {
      list
    }
    case _ if (level == rows / 2) => {
      var layer = Layer()
      for (j <- level to (cols - level - 1)) {
        layer = layer.+:((level, j))
      }
      list.::(layer.reverse)
    }
    case _ if (level == cols / 2) => {
      var layer = Layer()
      for (i <- level to (rows - level - 1)) {
        layer = layer.+:((i, level))
      }
      list.::(layer.reverse)
    }
    case _ => list
  }

  def loadIntoLists(array: Array[Array[Int]], cols: Int, list: Layers): Layers = {
    peel(array, 0, array.length, cols, list)
  }

  def rotate(list: Layers, rows: Int, cols: Int, rotations: Int): Layers = {
    @tailrec
    def go(list: Layers, acc: Layers): Layers = list match {
      case h :: t => {
        val r = rotations % h.length
        go(t, rollList(h, r) :: acc)
      }
      case _ => acc
    }

    go(list, Layers()).reverse
  }

  def main(args: Array[String]) {
    val firstLine = StdIn.readLine().split(" ")
    //    val firstLine = "10 8 40".split(" ")
    val rows = firstLine(0).toInt
    val cols = firstLine(1).toInt
    val rotations = firstLine(2).toInt

    val array = loadArray(rows, cols)
    val list = loadIntoLists(array, cols, Layers())
    val rotated = rotate(list, rows, cols, rotations)

    val tuples = list.zip(rotated).map(i => i._1.zip(i._2))

    var newArray = Array.ofDim[Int](rows, cols)
    tuples.foreach(layer => {
      layer.foreach(item => {
        newArray(item._1._1)(item._1._2) = array(item._2._1)(item._2._2)
      })
    })

    printArray(newArray)
  }
}