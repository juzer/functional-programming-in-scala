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
//      arr(i) = StdIn.readLine().split(" ").map(_.toInt)
      arr(i) = List.range(1, cols + 1).map(_ + (i * cols)).mkString(" ").split(" ").map(_.toInt)
    }
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
        layer = layer.+:((i, rows - level - 1))
      }
      for (j <- level to (cols - level - 2)) {
        layer = layer.+:((rows - level - 1, cols - j - 2))
      }
      for (i <- level to (rows - level - 3)) {
        layer = layer.+:((rows - i  - 2, level))
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
      for(j <- level to (cols - level -1)) {
        layer = layer.+:((level, j))
      }
      list.::(layer.reverse)
    }
    case _ if (level == cols / 2) => {
      var layer = Layer()
      for(i <- level to (rows - level - 1)) {
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
    val c = (rows - 1) * 2 + (cols - 1) * 2
    // circumference
    val r = rotations % (c - 1) // # of rotations
    @tailrec
    def go(list: Layers, acc: Layers): Layers = list match {
      case h :: t => {
        go(t, rollList(h, r) :: acc)
      }
      case _ => acc
    }
    go(list, Layers()).reverse
  }

  def main(args: Array[String]) {
    //     val firstLine = StdIn.readLine().split(" ")
    val firstLine = "4 4 1".split(" ")
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