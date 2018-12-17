package property

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_).forSize(n) }
    }
    SGen(g2)
  }

  def map2[B, C](b: Gen[B]) (f:(A, B) => C): SGen[C]=
    SGen { forSize(_).map2(b)(f)}

  def **[B](g: Gen[B]): SGen[(A,B)] =
    SGen { forSize(_) ** g}
}
