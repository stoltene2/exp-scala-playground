
object Ch2 {
  def fib(n: Int): Int = {

    def go(rem: Int, cur: Int, next: Int): Int = {
      if (rem == 0) cur
      else go(rem - 1, next, cur + next)
    }

    go(n, 1, 1)
  }
}
