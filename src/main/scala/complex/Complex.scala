package complex

import scala.math.Ordering

import scala.language.implicitConversions

case class Complex(re: Double, im: Double) {
  (lhs) =>

  def +(rhs: Complex): Complex = Complex(lhs.re + rhs.re, lhs.im + rhs.im)

  def -(rhs: Complex): Complex = Complex(lhs.re - rhs.re, lhs.im - rhs.im)

  def *(rhs: Complex): Complex = {
    val re = lhs.re * rhs.re - lhs.im * rhs.im
    val im = lhs.re * rhs.im + rhs.re * lhs.im
    Complex(re, im)
  }

  def *(alpha: Double): Complex = Complex(alpha * re, alpha * im)

  def /(rhs: Complex): Complex = lhs * rhs.reciprocal

  def exp: Complex =
    Complex(Math.exp(re) * Math.cos(im), Math.exp(re) * Math.sin(im))

  def sin: Complex =
    Complex(Math.sin(re) * Math.cosh(im), Math.cos(re) * Math.sinh(im))

  def cos: Complex =
    Complex(Math.cos(re) * Math.cosh(im), -Math.sin(re) * Math.sinh(im))

  def tan: Complex =
    sin / cos

  def conjugate: Complex = Complex(re, -im)

  def reciprocal: Complex = {
    val scale = re * re + im * im
    Complex(re / scale, -im / scale)
  }

  def abs: Double = Math.hypot(re, im)

  def phase: Double = Math.atan2(im, re)

  override def toString: String = (re, im) match {
    case (_, 0) => re.toString
    case (0, _) => s"${im}i"
    case _ if im < 0 => s"${re} - ${-im}i"
    case _ => s"${re} + ${im}i"
  }
}

object Complex {
  val zero = Complex(0, 0)

  def real(re: Double) = Complex(re, 0)

  def imaginary(im: Double) = Complex(0, im)

  object ComplexAsNumeric extends Numeric[Complex] {
    override def plus(x: Complex, y: Complex): Complex = x + y

    override def minus(x: Complex, y: Complex): Complex = x - y

    override def times(x: Complex, y: Complex): Complex = x * y

    override def negate(x: Complex): Complex = zero - x

    override def fromInt(x: Int): Complex = real(x)

    override def toInt(x: Complex): Int = x.abs.toInt

    override def toLong(x: Complex): Long = x.abs.toLong

    override def toFloat(x: Complex): Float = x.abs.toFloat

    override def toDouble(x: Complex): Double = x.abs

    override def compare(x: Complex, y: Complex): Int =
      Ordering.Double.compare(x.abs, y.abs)
  }

  object Implicits {
    implicit class DoubleToImaginary(val d:Double) extends AnyVal {
      def i: Complex = Complex.imaginary(d)
    }

    implicit def doubleToComplex(d: Double): Complex = Complex.real(d)
  }
}
