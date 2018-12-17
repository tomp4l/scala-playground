package pure

import State._

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] =
    int.map(math.abs(_) match {
      case a if a > 0 => a
      case _ => 0
    })

  def randomPair: Rand[(Int, Int)] = both(int, int)

  def double: Rand[Double] =
    nonNegativeInt.map(_.toDouble / (Int.MaxValue.toDouble + 1.0))

  def intDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double.run(rng)
    val (d2, rng3) = double.run(rng2)
    val (d3, rng4) = double.run(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(
      i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
      }
    )

  def rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)

  def boolean: Rand[Boolean] = nonNegativeLessThan(2).map(_ > 0)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }
}
