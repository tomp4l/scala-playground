package monoid

import parallelism.Par
import parallelism.Par.Par
import parser.Parsers
import parser.Parsers.Parser
import property.Gen
import pure.State

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}


trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(a => a)

  object Laws {
    def associative[A](f: A => F[A], g: A => F[A], h: A => F[A], x: A): Boolean =
      compose(compose(f, g), h)(x) == compose(f, compose(g, h))(x)

    def identity[A](f: A => F[A], x: A): Boolean =
      compose(f, (a: A) => unit(a))(x) == f(x) &&
        compose((a: A) => unit(a), f)(x) == f(x)
  }

//  def composeM[M[_], N[_]](M: Monad[M], N: Monad[N], T: Traverse[N]):
//  Monad[({type f[x] = M[N[x]]})#f] = new Monad[({type f[x] = M[N[x]]})#f] {
//    override def flatMap[A, B](mna: M[N[A]])(f: A => M[N[B]]): M[N[B]] =
//      M.flatMap(mna)(na => N.map(T.traverse(na)(f)(M))(N.join))
//
//    override def unit[A](a: => A): M[N[A]] =
//      M.unit(N.unit(a))
//  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }


  object Laws {
    def identity[A, F[_]](functor: Functor[F], x: F[A]): Boolean =
      functor.map(x)(a => a) == x
  }

}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
      fa flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa flatMap f
  }

  val parMonad: Monad[Par.Par] = new Monad[Par.Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(fa)(f)
  }

  def parserMonad[P[+ _]](parsers: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] =
      parsers.succeed(a)

    override def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
      parsers.flatMap(fa)(f)
  }

  def stateMonad[S]: Monad[({type s[A] = State[S, A]})#s] = {
    class StateS {
      type s[A] = State[S, A]
    }
    new Monad[StateS#s] {
      def unit[A](a: => A): State[S, A] = State.unit(a)

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)

    def map[B](f: A => B): Id[B] = Id(f(value))
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      fa.flatMap(f)

    override def map[A, B](ma: Id[A])(f: A => B): Id[B] =
      ma.map(f)
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        fa flatMap f

      override def unit[A](a: => A): Either[E, A] =
        Right(a)
    }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] =
    new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
}

