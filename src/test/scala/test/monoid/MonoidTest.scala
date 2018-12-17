package test.monoid

import java.util.concurrent.Executors

import monoid.Monoid
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import parallelism.Par

class MonoidTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  it should "implement the string monoid" in {
    forAll {
      (a: String, b: String, c: String) => {
        Monoid.Laws.associative(Monoid.stringMonoid, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.stringMonoid, a) should be(true)
      }
    }
  }

  it should "implement the list monoid" in {
    forAll {
      (a: List[Int], b: List[Int], c: List[Int]) => {
        Monoid.Laws.associative(Monoid.listMonoid, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.listMonoid, a) should be(true)
      }
    }
  }

  it should "implement the option monoid" in {
    forAll {
      (a: Option[Int], b: Option[Int], c: Option[Int]) => {
        Monoid.Laws.associative(Monoid.optionMonoid[Int], a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.optionMonoid[Int], a) should be(true)
      }
    }
  }

  it should "implement the integer monoids" in {
    forAll {
      (a: Int, b: Int, c: Int) => {
        Monoid.Laws.associative(Monoid.intAddition, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.intAddition, a) should be(true)
        Monoid.Laws.associative(Monoid.intMultiplication, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.intMultiplication, a) should be(true)
      }
    }
  }

  it should "implement the boolean monoids" in {
    forAll {
      (a: Boolean, b: Boolean, c: Boolean) => {
        Monoid.Laws.associative(Monoid.booleanOr, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.booleanOr, a) should be(true)
        Monoid.Laws.associative(Monoid.booleanAnd, a, b, c) should be(true)
        Monoid.Laws.zeroes(Monoid.booleanAnd, a) should be(true)
      }
    }
  }

  it should "implement the endo monoids" in {
    forAll {
      (a: Int => Int, b: Int => Int, c: Int => Int, i: Int) => {
        val monoid = Monoid.endoMonoid[Int]
        monoid.op(a, monoid.op(b, c))(i) should be(monoid.op(monoid.op(a, b), c)(i))
        monoid.op(monoid.zero, a)(i) should be(a(i))
        monoid.op(a, monoid.zero)(i) should be(a(i))
      }
    }
  }

  private val wcGenerator = Gen.oneOf[Monoid.WC](
    Gen.resultOf(Monoid.Stub),
    Gen.resultOf(Monoid.Part)
  )

  it should "implement the wc monoid" in {
    forAll(wcGenerator, wcGenerator, wcGenerator) {
      (a: Monoid.WC, b: Monoid.WC, c: Monoid.WC) => {
        val monoid = Monoid.wcMonoid
        monoid.op(a, monoid.op(b, c)) should be(monoid.op(monoid.op(a, b), c))
        monoid.op(monoid.zero, a) should be(a)
        monoid.op(a, monoid.zero) should be(a)
      }
    }
  }

  it should "fold balanced" in {
    val a = Array(1, 2, 3, 4, 5)
    Monoid.foldMapV(a, Monoid.intAddition)(a => a) should be(15)
  }

  it should "fold balanced in parallel" in {
    val executor = Executors.newFixedThreadPool(10)
    val a = Array(1, 2, 3, 4, 5)
    Par.run(executor)(Monoid.parFoldMap(a, Monoid.intAddition)(a => a)) should be(15)
  }

  it should "count the words in a string" in {
    Monoid.count("") should be(0)
    Monoid.count("sdfsdf") should be(1)
    Monoid.count("sdfsdf sdfsdf sdfsdf") should be(3)
    Monoid.count("       ") should be(0)
  }

  it should "prove tuples of monoids are also monoids" in {
    type tuple = (Int, String)
    forAll {
      (a: tuple, b: tuple, c: tuple) => {
        val monoid = Monoid.productMonoid(Monoid.intAddition, Monoid.stringMonoid)
        Monoid.Laws.associative(monoid, a, b, c) should be(true)
        Monoid.Laws.zeroes(monoid, a) should be(true)
      }
    }
  }
}
