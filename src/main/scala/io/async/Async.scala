package io.async


import java.util.concurrent.Executors

import io.Async
import io.free.{FlatMap, Return, Suspend}
import monoid.Monad
import parallelism.Par
import parallelism.Par.Par

object Async extends Monad[Async] {
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case g@FlatMap(f@FlatMap(x, _), _) =>
      step(x.asInstanceOf[Async[A]] flatMap (a => f.k(a) flatMap g.k))
    case f@FlatMap(Return(x), _) => step(f.k(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(Return(a)))
    case f@FlatMap(x, _) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f.k(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  override def flatMap[A, B](fa: Async[A])(f: A => Async[B]): Async[B] =
    fa flatMap f

  override def unit[A](a: => A): Async[A] =
    Return(a)

  def suspend[A](a: => A): Async[A] =
    Suspend(Par.lazyUnit(a))
}

object Main {
  def main(args: Array[String]): Unit = {
    val one = Async.unit(1)

    val four = one.map(_ + 1).flatMap(
      a => Async.suspend(a * 2)
    )

    val par = Async.run(four flatMap (a => Async.unit(println(a))))

    Par.run(Executors.newFixedThreadPool(2))(par)
  }
}