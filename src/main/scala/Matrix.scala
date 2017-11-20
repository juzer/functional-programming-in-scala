import scala.annotation.tailrec
import scala.io.StdIn

object Solution {

  type Layers = List[List[Int]]
  def Layers() = List[List[Int]]()

  def loadArray(rows: Int, cols: Int): Array[Array[Int]] = {
    val arr = Array.ofDim[Int](rows, cols)
    for (i <- 0 to rows - 1) {
      arr(i) = StdIn.readLine().split(" ").map(_.toInt)
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

  def peel(array: Array[Array[Int]], cols: Int, list: Layers): (Layers, Array[Array[Int]]) = {
    var layer = List[Int]()
    layer = layer ++ array(0)
    layer = layer ++ array.map {
      _ (cols - 1)
    }.splitAt(1)._2.splitAt(array.length - 2)._1
    layer = layer ++ array(array.length - 1).reverse
    layer = layer ++ array.map {
      _ (0)
    }.splitAt(1)._2.splitAt(array.length - 2)._1.reverse

    var tempArray = array.splitAt(1)._2.splitAt(array.length - 2)._1
    for (i <- 0 to tempArray.length - 1) {
      tempArray(i) = tempArray.last.splitAt(1)._2.splitAt(cols - 2)._1
    }
//    if (list.head == Nil) (Layers(layer), tempArray)
//    else
    ((list :: layer).asInstanceOf[Layers], tempArray)
  }

  def loadIntoLists(array: Array[Array[Int]], cols: Int, list: Layers): Layers = cols match {
    case x if x > 1 => {
      val (newList, newArr) = peel(array, cols, list)
      loadIntoLists(newArr, cols - 2, newList)
    }
    case x if x == 1 => {
      list :: List(array(0)(0))
      list
    }
    case _ => {
      if (list.head == Nil) {
        list.splitAt(1)._2
      } else list
    }
  }

  def rotate(list: Layers, rows: Int, cols: Int, rotations: Int): Layers = {
    val c = (rows - 1) * 2 + (cols - 1) * 2
    // circumference
    val r = rotations % (c - 1) // # of rotations
    @tailrec
    def go(list: Layers, acc: Layers): Layers = list match {
      case h :: t => {
        go(t, (acc :: rollList(h, r)).asInstanceOf[Layers])
      }
      case _ => acc
    }
    go(list, Layers())
  }

  def fillArray(list: Layers, minRow: Int, maxRow: Int, minCol: Int, maxCol: Int, array: Array[Array[Int]]): Array[Array[Int]] = list match {
    case h :: t => {
      val temp = h.splitAt(maxCol - minCol)
      array(minRow) = temp._1.toArray
      var internal = temp._2
      for (i <- 1 to maxRow - 1) {
        array(i)(0) = internal.last
        internal = internal.dropRight(1)
        array(i)(maxCol) = internal(0)
        internal = internal.drop(1)
      }
      array(maxRow) = internal.reverse.toArray
      fillArray(t, minRow + 1, maxRow - 1, minCol + 1, maxCol - 1, array)
    }
    case _ => array
  }

  def toArray(list: Layers, rows: Int, cols: Int): Array[Array[Int]] = {
    val array = Array.ofDim[Int](rows, cols)
    fillArray(list, 0, rows -1, 0, cols - 1, array)
  }

  def main(args: Array[String]) {
    val firstLine = StdIn.readLine().split(" ")
    val rows = firstLine(0).toInt
    val cols = firstLine(1).toInt
    val rotations = firstLine(2).toInt

    val array = loadArray(rows, cols)
    val list = loadIntoLists(array, cols, Layers())

    printArray(toArray(rotate(list, rows, cols, rotations), rows, cols))
  }
}