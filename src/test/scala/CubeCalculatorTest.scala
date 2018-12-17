import example._

import collection.mutable.Stack
import org.scalatest._
import org.scalatest.prop._

class CubeCalculatorTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "CubeCalculator" should "calculate a cube" in {
    CubeCalculator.cube(3) should be (27)
  }

  it should "calculate the cube of 0" in {
    CubeCalculator.cube(0) should be (0)
  }

  forAll {
    (n: Int) => {
      whenever(n > 0) {
        CubeCalculator.cube(n) should be > 0
      }
      whenever(n < 0) {
        CubeCalculator.cube(n) should be < 0
      }
    }
  }
}