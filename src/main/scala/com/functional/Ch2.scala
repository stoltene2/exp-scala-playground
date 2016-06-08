
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

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a,b)
    }
  }

  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (x: A) => f(g(x))
  }
}
