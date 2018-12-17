package io.console

import io.free.{FlatMap, Free, Return, Suspend}
import monoid.Monad
import parallelism.Par
import parallelism.Par.Par

sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A

  def toReader: ConsoleReader[A]
}

case object ReadLine extends Console[Option[String]] {
  def toPar: Par[Option[String]] = Par.lazyUnit(run)

  def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try Some(scala.io.StdIn.readLine())
    catch {
      case _: Exception => None
    }

  def toReader = ConsoleReader { in => Some(in) }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar: Par[Unit] = Par.lazyUnit(println(line))

  def toThunk: () => Unit = () => println(line)

  def toReader = ConsoleReader { s => () } // noop
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0: Console ~> Function0 =
    new (Console ~> Function0) {
      def apply[A](a: Console[A]) = a.toThunk
    }

  val consoleToPar: Console ~> Par =
    new (Console ~> Par) {
      def apply[A](a: Console[A]) = a.toPar
    }

  import io._

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case g@FlatMap(f@FlatMap(x, _), _) => step(x flatMap (a => f.k(a) flatMap g.k))
    case f@FlatMap(Return(x), _) => step(f.k(x))
    case _ => a
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case f@FlatMap(Suspend(r), _) => G.flatMap(t(r))(a => runFree(f.k(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[B] = Free[G, B]
    val t = new (F ~> FreeG) {
      def apply[B](a: F[B]): Free[G, B] = Suspend {
        fg(a)
      }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(consoleToFunction0))

  val consoleToReader: Console ~> ConsoleReader =
    new (Console ~> ConsoleReader) {
      def apply[A](a: Console[A]) = a.toReader
    }


  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](io)(consoleToReader)
}

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)))

  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)).run(r))
}

object ConsoleReader {
  implicit val monad: Monad[ConsoleReader] = new Monad[ConsoleReader] {
    def unit[A](a: => A) = ConsoleReader(_ => a)

    def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] =
      ra flatMap f
  }
}