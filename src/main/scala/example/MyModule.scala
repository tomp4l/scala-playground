package example

import scala.annotation.tailrec

object MyModule {

  /**
    * Exercise 2.1
    *
    * @param n
    * @return
    */
  def fibonacci(n: Int): Int = {
    @tailrec
    def aux(a: Int, b: Int, term: Int): Int = {
      if (term == n) a
      else aux(b, a + b, term + 1)
    }

    aux(0, 1, 0)
  }

  /**
    * Exercise 2.2
    *
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n + 1), as(n))) loop(n + 1)
      else false
    }

    loop(0)
  }

  /**
    * Exercise 2.3
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a) => (b) => f(a, b)
  }

  /**
    * Exercise 2.4
    *
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Exercise 2.5
    *
    * @param f
    * @param g
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val l = xs.length
    if (l == 0) {
      None
    } else {
      val m = xs.sum / l
      Some(
        xs
          .map(x => math.pow(x - m, 2))
          .sum
          / l
      )
    }
  }
}
