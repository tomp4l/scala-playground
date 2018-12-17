package test.pure

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import pure.RNG

case class ReturnRng(value: Int) extends RNG {
  def nextInt: (Int, RNG) = (value, this)
}

case class ReturnPlusOneRng(value: Int) extends RNG {
  def nextInt: (Int, RNG) = (value, ReturnPlusOneRng(value + 1))
}

case class HalvingRng(value: Int) extends RNG {
  def nextInt: (Int, RNG) = (value, ReturnPlusOneRng(value / 2))
}

class RngTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  forAll {
    n: Int => RNG.nonNegativeInt.run(ReturnRng(n))._1 should be >= 0
  }

  forAll {
    n: Int => RNG.double.run(ReturnRng(n))._1 should (be >= 0.0 and be < 1.0)
  }

  forAll {
    (n: Int, lessThan: Int) =>
      whenever(lessThan > 0) {
        RNG.nonNegativeLessThan(lessThan).run(HalvingRng(n))._1 should
          (be >= 0 and be < lessThan)
      }
  }

  forAll {
    n: Int => RNG.rollDie.run(HalvingRng(n))._1 should (be >= 1 and be <= 6)
  }

  it should "generate random lists of ints" in {
    RNG.ints(4).run(ReturnPlusOneRng(1))._1 should be(List(4, 3, 2, 1))
  }

  it should "generate pairs of int and doubles" in {
    RNG.intDouble.run(ReturnRng(0))._1 should be(0 -> 0.0)
    RNG.doubleInt.run(ReturnRng(0))._1 should be(0.0 -> 0)
  }
}
