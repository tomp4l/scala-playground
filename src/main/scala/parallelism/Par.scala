package parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Par {

  sealed trait Future[A] {
    private[parallelism] def apply(k: A => Unit, onError: Throwable => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]


  def unit[A](a: A): Par[A] =
    _ => new Future[A] {
      def apply(cb: A => Unit, ecb: Throwable => Unit): Unit = cb(a)
    }

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit, ecb: Throwable => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)), ecb)
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)), ecb)
          }
        }
        p(es)(a => combiner ! Left(a), ecb)
        p2(es)(b => combiner ! Right(b), ecb)
      }
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)(_ -> _), c)((ab, cc) => f(ab._1, ab._2, cc))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map2(a, b)(_ -> _), map2(c, d)(_ -> _))((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def run[A](es: ExecutorService)(a: Par[A]): A = {
    val ref = new AtomicReference[A]
    val error = new AtomicReference[Throwable]
    val latch = new CountDownLatch(1)
    a(es)({
      a =>
        ref.set(a)
        latch.countDown()
    }, {
      e =>
        error.set(e)
        latch.countDown()
    })
    latch.await()
    Option(error.get) match {
      case Some(e) => throw e
      case None => ()
    }
    ref.get
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit, ecb: Throwable => Unit): Unit =
        eval(es)(a(es)(cb, ecb), ecb)
    }

  def eval(es: ExecutorService)(r: => Unit, ecb: Throwable => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = try {
        r
      } catch {
        case t: Throwable => ecb(t)
      }
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))


  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(List.empty[A]))((acc, v) => map2(v, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val parMapped = parMap(as)(a => Some(a).filter(f))
      map(parMapped)(_.flatten)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def reduce[A](seq: IndexedSeq[A], z: A)(f: (A, A) => A): Par[A] =
    if (seq.length <= 1)
      Par.unit(seq.headOption getOrElse z)
    else {
      val (l, r) = seq.splitAt(seq.length / 2)
      Par.map2(Par.fork(reduce(l, z)(f)), Par.fork(reduce(r, z)(f)))(f)
    }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices(_))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices(_))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es =>
      join(map(pa)(f(_)))(es)

  def join[A](a: Par[Par[A]]): Par[A] =
    es =>
      run(es)(a)(es)

  def map2sync[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    val curry = (a: A) => unit((b: B) => unit(f(a, b)))
    flatMap(flatMap(p)(curry))(f => flatMap(p2)(f))
  }
}
