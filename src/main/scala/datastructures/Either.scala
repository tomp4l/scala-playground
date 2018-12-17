package datastructures

sealed trait Either[+E, +A] {
  self =>
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    self.flatMap(a => b.map(f(a, _)))
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  self =>

  override def map[B](f: Nothing => B): Either[E, B] = self

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = self

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  self =>

  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] =
    f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] =
    self
}


object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRight(as, Right(Nil): Either[E, List[B]])(
      (v, l) => f(v).map2(l)(Cons(_, _))
    )
}