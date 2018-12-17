import io.free.{FlatMap, Free, Return, Suspend}
import monoid.Monad
import parallelism.Par
import parallelism.Par.Par

import scala.language.higherKinds

package object io {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        fa flatMap f

      override def unit[A](a: => A): Free[F, A] = Return(a)
    }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(a) => a
      case Suspend(s) => s()
      case f@FlatMap(fs, _) => fs match {
        case Return(a) => runTrampoline(f.k(a))
        case Suspend(s) =>
          runTrampoline(f.k(s()))
        case g@FlatMap(s, _) =>
          runTrampoline(s flatMap (b => g.k(b).flatMap(f.k)))
      }
    }


  def run[F[_], A](free: Free[F, A])(implicit F: Monad[F]): F[A] =
    free match {
      case Return(a) => F.unit(a)
      case Suspend(s) => s
      case f@FlatMap(fs, _) => fs match {
        case Return(a) => run(f.k(a))
        case Suspend(s) =>
          F.flatMap(s)(a => run(f.k(a)))
        case g@FlatMap(s, _) =>
          run(s flatMap (a => g.k(a) flatMap f.k))
      }
    }

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): () => A = () => a

    def flatMap[A, B](a: () => A)(f: A => () => B): () => B =
      () => f(a())()
  }

  implicit val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      Par.fork {
        Par.flatMap(a)(f)
      }
  }
}