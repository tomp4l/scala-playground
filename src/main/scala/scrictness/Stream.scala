package scrictness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] =
    foldRight(Option.empty[A])((v, _) => Some(v))

  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], l: List[A]): List[A] =
      s match {
        case Empty => l.reverse
        case Cons(h, t) => loop(t(), h() :: l)
      }

    loop(this, List.empty)
  }

  def take(n: Int): Stream[A] = {
    Stream.unfold((n, this)) {
      case (s, Cons(h, t)) if s > 0 => Some(h(), (s - 1, t()))
      case _ => None
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    zipWith(bs)(_ -> _)

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
      case _ => None
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(
        (Some(ah()), Some(bh())),
        (at(), bt())
      )
      case (Cons(ah, at), Empty) => Some(
        (Some(ah()), None),
        (at(), Empty)
      )
      case (Empty, Cons(bh, bt)) => Some(
        (None, Some(bh())),
        (Empty, bt())
      )
      case _ => None
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])(
      (h, tl) =>
        if (f(h)) Stream.cons(h, tl)
        else tl
    )

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((h, tl) => Stream.cons(h, tl))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, tl) => f(a) append tl)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).foldRight(true)(
      (v, acc) => v match {
        case (Some(a), Some(b)) if a == b => acc
        case (_, None) => true
        case _ => false
      }
    )

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val acc1 = acc
      val b = f(a, acc1._1)
      (b, Stream.cons(b, acc1._2))
    })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def ones: Stream[Int] = {
    unfold(1)(s => Some((s, s)))
  }

  def constant[A](a: => A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }

  def from(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def fibs: Stream[Int] = {
    unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(a: S): Stream[A] = {
      f(a).map(
        v => cons(v._1, loop(v._2))
      ).getOrElse(empty[A])
    }

    loop(z)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}