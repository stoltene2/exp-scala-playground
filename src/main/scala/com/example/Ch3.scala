import scala.util.Random
import scala.util.Sorting.{quickSort}

object Ch3 {

  // Like Haskell, except we can interact with the outside world here
  def randArray(size: Int): Array[Int] = {
    val randArray = new Array[Int](size)

    for (i <- 0 until size) { randArray(i) = Random.nextInt(size) }

    randArray
  }

  // to test this in REPL
  // > val t = Array(1 to 7: _*)
  // > Ch3.swapAdjacentDumb
  def swapAdjacentDumb(orig: Array[Int]): Unit = {
    var tmp = 0
    for (i <- 0.until(orig.length - 1, 2)) {
      tmp = orig(i)
      orig(i) = orig(i+1)
      orig(i+1) = tmp
    }
  }

  def swapAdjacentSmart(orig: Array[Int]): Array[Int] = {
    val result = for (i <- 0 until orig.length) yield {
      if (i == orig.length - 1 && orig.length % 2 == 1 ) orig(i)
      else if (i % 2 == 0) orig(i+1)
      else orig(i-1)
    }
    result.toArray
  }

  def partitionNeg(orig: Array[Int]): Array[Int] = {
    val (neg, pos) = orig partition (_ < 0)
    pos ++ neg
  }

  def average(as: Array[Double]): Double = {
    as.sum / as.length
  }

  def reverseSorted(as: Array[Int]): Unit = {
    quickSort(as)
    // for (i <- 0 to (as.length / 2) ) {
    //   val tmp = a(i)
    //   a(i) = a(as.length - 1 - i)
    //   a(as.length - 1 - i) = tmp
    // }
  }

}
