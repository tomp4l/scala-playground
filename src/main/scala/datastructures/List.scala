package datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def loop(acc: List[A], asTail: List[A], bsTail: List[A]): List[A] = {
      (asTail, bsTail) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(a, aTl), Cons(b, bTl)) => loop(Cons(f(a, b), acc), aTl, bTl)
      }
    }
    List.rev(loop(Nil, as, bs))
  }

  def addTwo(as: List[Int], bs: List[Int]): List[Int] = {
    List.zipWith(as, bs)(_ + _)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    List.flatten(List.map(as)(f))
  }

  def filter2[A](list: List[A])(f: A => Boolean): List[A] = {
    flatMap(list)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    List.foldRightUsingLeft(list, Nil: List[A])((value, acc) =>
      if (f(value)) {
        Cons(value, acc)
      } else {
        acc
      }
    )
  }

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    List.foldRightUsingLeft(list, Nil: List[B])((value, acc) => {
      Cons(f(value), acc)
    })
  }

  def add[A: Numeric](list: List[A], i: A): List[A] = {
    import Numeric.Implicits._
    List.map(list)(_ + i)
  }

  def toString(list: List[Any]): List[String] = List.map(list)(_.toString)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, tl) => Cons(a, tl)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def doDrop(as: List[A], x: Int): List[A] = {
      if (x < n) as match {
        case Nil => Nil
        case Cons(_, tl) => doDrop(tl, x + 1)
      }
      else as
    }

    doDrop(l, 0)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def doDrop(as: List[A], x: Int): List[A] =
      as match {
        case Nil => Nil
        case Cons(hd, tl) =>
          if (f(hd)) doDrop(tl, x + 1)
          else as
      }

    doDrop(l, 0)
  }


  def rev[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def initRev(as: List[A], bs: List[A]): List[A] =
      as match {
        case Nil => bs
        case Cons(_, Nil) => bs
        case Cons(a, Cons(b, tl)) =>
          initRev(Cons(b, tl), Cons(a, bs))
      }

    rev(initRev(l, Nil))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(hd, tl) => foldLeft(tl, f(z, hd))(f)
  }

  def foldRightUsingLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(rev(as), z)((a, b) => f(b, a))
  }

  def append[A](as: List[A], bs: List[A]): List[A] = {
    foldRightUsingLeft(as, bs: List[A])(Cons(_, _))
  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((a, _) => a + 1)
  }

  def flatten[A](as: List[List[A]]): List[A] = {
    rev(
      foldLeft(as, Nil: List[A])(
        (acc, l) => foldLeft(l, acc)((a, b) => Cons(b, a))
      )
    )
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def doMatch(remainder: List[A], remainderMatch: List[A]): Boolean = {
      (remainder, remainderMatch) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, tlx), Cons(y, tly)) => if(x == y) {
          doMatch(tlx, tly)
        } else {
          false
        }
      }
    }
    @tailrec
    def loop(remainder: List[A]): Boolean = {
      remainder match {
        case Nil => false
        case Cons(_, tl) =>
          if (doMatch(remainder, sub)) {
            true
          } else {
            loop(tl)
          }
      }
    }
    loop(sup)
  }


  def flattenNotTailRec[A](as: List[List[A]]): List[A] =
    as match {
      case Nil => Nil
      case Cons(hd, tl) => append(hd, flattenNotTailRec(tl))
    }
}