package datastructures

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[A](value: A) extends Option[A] {
  self =>
  override def map[B](f: A => B): Option[B] = Some(f(value))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(value)

  override def getOrElse[B >: A](default: => B): B = value

  override def orElse[B >: A](ob: => Option[B]): Option[B] = self

  override def filter(f: A => Boolean): Option[A] = if (f(value)) {
    self
  } else {
    None
  }
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.length)
    }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(f(a, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Option.traverse(a)(a => a)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldRight(a, Some(Nil): Option[List[B]])(
      (v, l) => l.flatMap(tl => f(v).map(Cons(_, tl)))
    )
}