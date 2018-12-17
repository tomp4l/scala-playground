package streamio

import monoid.Monad

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def |>[O2](p2: Process[O, O2]): Process[I, O2] =
    p2 match {
      case Halt() => Halt()
      case Emit(h, tl) => Emit(h, this |> tl)
      case Await(r1) => this match {
        case Halt() => Halt() |> r1(None)
        case Emit(head, tail) => tail |> r1(Some(head))
        case Await(r2) => Await {
          i => r2(i) |> p2
        }
      }
    }

  def map[O2](f: O => O2): Process[I, O2] = this |> Process.lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
    this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

  def zipWithIndex: Process[I, (O, Int)] =
    this |> Process.loop(0) {
      case (o, i) => ((o, i), i + 1)
    }

  def zip[O2](p: => Process[I, O2]): Process[I, (O, O2)] =
    (this, p) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (Emit(h, t), Emit(h2, t2)) => Emit((h, h2), t zip t2)
      case (Emit(_, _), Await(g)) => Await(i => this zip g(i))
      case (Await(f), Emit(_, _)) => Await(i => f(i) zip p)
      case (Await(f), Await(g)) => Await(i => f(i) zip g(i))
    }

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](receive: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]


object Process {
  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] =
    liftOne(f).repeat

  def await[I, O](f: I => Process[I, O]): Await[I, O] =
    Await {
      case Some(i) => f(i)
      case None => Halt()
    }

  def take[I](n: Int): Process[I, I] = {
    def go(acc: Int): Process[I, I] =
      await {
        case i if acc < n => Emit(i, go(acc + 1))
        case _ => Halt()
      }

    go(0)
  }

  def id[I]: Process[I, I] = lift(identity)

  def drop[I](n: Int): Process[I, I] = {
    def go(acc: Int): Process[I, I] =
      await {
        case _ if acc < n => go(acc + 1)
        case i => Emit(i, id)
      }

    go(0)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    def go(): Process[I, I] =
      await {
        case i if f(i) => Emit(i, go())
        case _ => Halt()
      }

    go()
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    def go(): Process[I, I] =
      await {
        case i if f(i) => go()
        case i => Emit(i, id)
      }

    go()
  }

  def count[I]: Process[I, Int] =
    lift((_: I) => 1.0) |> sum |> lift(_.toInt)

  def sum: Process[Double, Double] =
    loop(0.0) {
      case (i, s) => (i + s, i + s)
    }

  def mean: Process[Double, Double] =
    count zip sum map { case (c, s) => s / c.toDouble }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await({
      i =>
        val (o, s) = f(i, z)
        Emit(o, loop(s)(f))
    })

  def exists[I](f: I => Boolean): Process[I, Boolean] =
    lift(f) |> loop(false) { case (v, b) => (v || b, v || b) }

  def monad[I]: Monad[({type f[x] = Process[I, x]})#f] =
    new Monad[({type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)

      def flatMap[O, O2](p: Process[I, O])(
        f: O => Process[I, O2]): Process[I, O2] =
        p flatMap f
    }

}