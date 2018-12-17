package monoid

import parallelism.Par

import scala.collection.Seq
import scala.math.Ordering

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, v) => m.op(acc, f(v)))

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v match {
      case Seq() => m.zero
      case Seq(a) => f(a)
      case Seq(a, b) => m.op(f(a), f(b))
      case _ =>
        val (left, right) = v.splitAt(v.length / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par.Par[A]] = new Monoid[Par.Par[A]] {
    override def op(a1: Par.Par[A], a2: Par.Par[A]): Par.Par[A] =
      Par.map2(a1, a2)(m.op)

    override def zero: Par.Par[A] =
      Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par.Par[B] = {
    foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(c), Stub(d)) => Stub(c + d)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
      }

    override def zero: WC = Stub("")
  }

  def count(words: String): Int = {
    def wc(char: Char): WC =
      if (char.isWhitespace) {
        Part("", 0, "")
      } else {
        Stub(char.toString)
      }

    val wcs = words.toCharArray
    foldMapV(wcs, wcMonoid)(wc) match {
      case Stub(s) => s.length.min(1)
      case Part(a, w, b) => a.length.min(1) + w + b.length.min(1)
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val listMonoid: Monoid[List[_]] = new Monoid[List[_]] {
    override def op(a1: List[_], a2: List[_]): List[_] = a1 ++ a2

    override def zero: List[_] = List.empty
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.orElse(a2)

    override def zero: Option[A] = Option.empty[A]
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A =
      a1 andThen a2

    override def zero: A => A = a => a
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(t1: (A, B), t2: (A, B)): (A, B) =
        (t1, t2) match {
          case ((a1, b1), (a2, b2)) => (A.op(a1, a2), B.op(b1, b2))
        }

      override def zero: (A, B) = (A.zero, B.zero)
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      } }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B =
      a => B.op(a1(a), a2(a))

    override def zero: A => B = _ => B.zero
  }

  object Laws {
    def associative[A](monoid: Monoid[A], a: A, b: A, c: A): Boolean =
      monoid.op(a, monoid.op(b, c)) == monoid.op(monoid.op(a, b), c)

    def zeroes[A](monoid: Monoid[A], a: A): Boolean =
      a == monoid.op(a, monoid.zero) &&
        monoid.op(monoid.zero, a) == a
  }

}