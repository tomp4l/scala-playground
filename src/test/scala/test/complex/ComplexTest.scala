package test.complex

import complex.Complex
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ComplexTest extends FlatSpec
  with GeneratorDrivenPropertyChecks
  with Matchers {

  private def s(complex: Complex) = complex.toString

  it should "add some numbers" in {
    s(Complex.zero + Complex(1, 0)) should be("1.0")
    s(Complex.zero + Complex(0, 1)) should be("1.0i")
    s(Complex(1, 2) + Complex(4, 1)) should be("5.0 + 3.0i")
  }

  forAll {
    (re: Double, im: Double) => {
      val j = Complex(re, im)
      val squared = Math.pow(j.abs, 2)
      whenever((squared !== 0.0) && (squared !== Double.PositiveInfinity)) {
        (j / j).abs should be(1.0 +- 0.001)
      }
    }
  }

  it should "implicitly convert" in {
    import Complex.Implicits._
    s(1.0 + 2.3.i) should be ("1.0 + 2.3i")


  }
}