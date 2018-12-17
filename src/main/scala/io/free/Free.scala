package io.free

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], k: A => Free[F, B]) extends Free[F, B]