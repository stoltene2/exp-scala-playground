
object Ch2 {

  def signum(n: Int): Int = {
    if (n > 0) 1
    else if (n == 0) 0
    else -1
  }

  def javaForLoop(): Unit = {
    for (i <- 10.to(0, -1)) {
      println(i)
    }
  }

  def countdown(n: Int): Unit = for (i <- n.to(0, -1)) { println(i)}

  def multiplyCharsDumb(str: String): Long = {
    // Obviously this isn't the way we should solve this, but good to
    // know we can.
    var result: Long = 1

    for (c <- str) { result *= c.toLong}

    result
  }

  def multiplyCharSmart(str: String): Long = {
    str.foldLeft(1: Long)((res, c) => res * c.toLong)
  }

  def multiplyCharRecursive(str: String): Long = {
    if (str.isEmpty) 1
    else str.head.toLong * multiplyCharRecursive(str.tail)
  }

  def calcPow(base: BigInt, pow: Int): BigDecimal = {
    if (pow == 0) 1.0
    else if (pow < 0) 1 / (calcPow(base, -pow))
    else if (pow % 2 == 0) calcPow(base, pow / 2) * calcPow(base, pow / 2)
    else BigDecimal(base) * calcPow(base, pow - 1)
  }
}
