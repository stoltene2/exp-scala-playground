
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(as: List[Int]): Int = {
    as match {
      case Nil => 0
      case Cons(x, rest) => x + sum(rest)
    }
  }

  def tail[A](as: List[A]): List[A] = as match {
    // Instead of throwing error just make it nil for now.
    case Nil => Nil
    case Cons(_, rest) => rest
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)
    }
  }

  def setHead[A](head: A, as: List[A]): List[A] = as match {
    case Nil => List(head)
    case Cons(_, tail) => Cons(head, tail)
  }

  // Note: This is subject to stack overflows
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
