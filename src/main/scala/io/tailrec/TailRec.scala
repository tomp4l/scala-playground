package io.tailrec

import io.TailRec
import io.free.{FlatMap, Return, Suspend}
import monoid.Monad

object TailRec extends Monad[TailRec] {
  def unit[A](a: => A): TailRec[A] = Return(a)
  def suspend[A](a: => A): TailRec[A] = Suspend(() => a)

  def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa flatMap f

  def apply[A](a: => A): TailRec[A] = unit(a)

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case f@FlatMap(sub, _) => sub match {
      case Return(a) => run(f.k(a))
      case Suspend(r) => run(f.k(r()))
      case f2@FlatMap(sub2, _) => run(
        sub2.flatMap(
          a => f2.k(a).flatMap(f.k)
        )
      )
    }
  }
}

case class Player(name: String, score: Int)

object Player {

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def PrintLine(msg: String): TailRec[Unit] =
    TailRec {
      println(msg)
    }

  def contest(p1: Player, p2: Player): TailRec[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))
}

object Converter {
  def ReadLine: TailRec[String] = TailRec {
    scala.io.StdIn.readLine()
  }

  def PrintLine(msg: String): TailRec[Unit] = TailRec {
    println(msg)
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: TailRec[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    TailRec.run(converter)
  }
}