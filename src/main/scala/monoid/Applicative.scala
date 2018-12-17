package monoid

import java.util.Date

import parser.Parsers
import pure.State

trait Applicative[F[_]] extends Functor[F] {
  self =>
  // primitive combinators
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))(
      (v, lifted) => map2(v, lifted)(_ :: _)
    )

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(
          (ga, gb) => G.map2(ga, gb)(f))
    }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))(
      (v, lifted) => map2(f(v), lifted)(
        (filter, l) =>
          if (filter) v :: l
          else l
      )
    )
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  case class Id[A](value: A)

  implicit val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] =
      Id(fab.value(fa.value))
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    val id: Id[F[B]] = traverse(fa)(a => Id(f(a)))
    id.value
  }

  import Applicative._

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(mb)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, l) => (l.head, l.tail))._1

//  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
//    mapAccum(as, z)((a, acc) => ((), f(a, acc)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1


  def fuse[G[_], H[_], A, B]
  (fa: F[A])(f: A => G[B], g: A => H[B])
  (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    val product = G product H
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(product)
  }

  def compose[T[_]](implicit T: Traverse[T]): Traverse[({type f[x] = F[T[x]]})#f] =
    new Traverse[({type f[x] = F[T[x]]})#f] {
      override def traverse[G[_] : Applicative, A, B](fa: F[T[A]])(f: A => G[B]): G[F[T[B]]] =
        self.traverse[G,T[A],T[B]](fa)((ta: T[A]) => T.traverse[G, A, B](ta)(f))
    }
}

object Traverse {
  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = {
      val applicative = implicitly[Applicative[G]]
      fga.fold(
        applicative.unit(Option.empty[A]))(
        applicative.map(_)(Some(_)))
    }
  }

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val applicative = implicitly[Applicative[G]]
      fa.foldRight(applicative.unit(List.empty[B]))(
        (v, l) => applicative.map2(f(v), l)(_ :: _)
      )
    }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail.map(map(_)(f)))

    override def sequence[G[_] : Applicative, A](fga: Tree[G[A]]): G[Tree[A]] = {
      val applicative = implicitly[Applicative[G]]
      applicative.map2(
        fga.head,
        listTraverse.traverse(fga.tail)(sequence(_))
      )(Tree(_, _))
    }
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  def validationApplicative[E]: Applicative[({type f[A] = Validation[E, A]})#f] = new Applicative[({type f[A] = Validation[E, A]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      fa match {
        case Success(a) => fb match {
          case Success(b) => Success(f(a, b))
          case f: Failure[E] => f
        }
        case f@Failure(ha, ta) => fb match {
          case Failure(hb, tb) => Failure(ha, (ta :+ hb) ++ tb)
          case _ => f
        }
      }
  }
}


object Main {

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")


  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    Applicative.validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(WebForm)

  def main(args: Array[String]): Unit = {
    println(validWebForm("test", "2000-01-01", "0712312312"))
    println(validWebForm("", "2000-01-01", "0712312312"))
    println(validWebForm("retard", "14/12/2010", "07123112"))
  }
}
