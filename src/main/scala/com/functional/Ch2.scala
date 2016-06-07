
object Ch2 {
  def fib(n: Int): Int = {

    def go(rem: Int, cur: Int, next: Int): Int = {
      if (rem == 0) cur
      else go(rem - 1, next, cur + next)
    }

    go(n, 1, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else as.zip(as.drop(1)).forall( x => ordered(x._1, x._2))
  }
}
