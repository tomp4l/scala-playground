package test.property

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class PropertyTest
  extends FlatSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  it should "sum the same in either order" in
    forAll {
      xs: List[Int] => {
        xs.sum should be(xs.reverse.sum)
      }
    }

  it should "sum to the number of items in the list if they are the same" in
    forAll(Arbitrary.arbitrary[Int], Gen.choose(0, 100)) {
      (x: Int, n: Int) =>
        whenever(n >= 0 && n < 100) {
          List.fill(n)(x).sum should be(x * n)
        }
    }

  it should "be a positive sum for a positive list" in
    forAll(Gen.listOf(Gen.choose(0, Int.MaxValue))) {
      xs: List[Int] =>
        xs.map(_.toLong).sum should be >= 0L
    }

  it should "give the same maximum for any order" in
    forAll {
      xs: List[Int] =>
        whenever(xs.nonEmpty) {
          xs.max should be(xs.reverse.max)
        }
    }

  it should "be the constant value for a constant value list" in {
    forAll(Arbitrary.arbitrary[Int], Gen.choose(1, 100)) {
      (x: Int, n: Int) =>
        List.fill(n)(x).max should be(x)
    }
  }

  it should "throw for an empty list" in
    assertThrows[UnsupportedOperationException](List.empty[Int].max)
}
