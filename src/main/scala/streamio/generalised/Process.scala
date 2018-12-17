package streamio.generalised

import monoid.Monad


trait Process[F[_], O] {

  import Process._

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] =
    this match {
      case Halt(e) => Try(f(e))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    }

  def ++(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req, recv) =>
      Await(req, recv andThen (_ flatMap f))
  }

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case a@Await(req, _) =>
          F.flatMap(F.attempt(req))(n => go(Try(a.recv(n)), acc))
      }

    go(this, IndexedSeq())
  }

  def asFinalizer: Process[F, O] =
    this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e) => Halt(e)
      case a@Await(req, _) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x => a.recv(x)
      }
    }

  def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }
}

object Process {

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O])
    extends Process[F, O]

  case class Emit[F[_], O](
    head: O,
    tail: Process[F, O]
  ) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception

  case object Kill extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  def eval[F[_], A](a: F[A]): Process[F, A] =
    await(a) {
      case Left(e) => Halt(e)
      case Right(value) => Emit(value, Halt(End))
    }

  def eval_[F[_], A, B](a: F[A]): Process[F, B] =
    await(a) {
      case Left(e) => Halt(e)
      case Right(_) => Halt(End)
    }
}


trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]

  def fail[A](t: Throwable): F[A]
}