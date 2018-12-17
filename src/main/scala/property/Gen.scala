package property

import pure.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.map(f).flatMap(_.sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](b: Gen[B]) (f:(A, B) => C): Gen[C]=
    this.flatMap(a => b.map(f(a,_)))

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
}

object Gen {
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(if (n > 0) n else 1, g))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeLessThan(stopExclusive - start).map(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(RNG.boolean)

  def double: Gen[Double] =
    Gen(RNG.double)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val weight = g1._2 / (g1._2 + g2._2)
    double.flatMap(d => if (d < weight) g1._1 else g2._1)
  }

  // TODO: make this better
  def string: Gen[String] = Gen.double.map(_.toString)
}