package test.parallelism

import java.util.concurrent.Executors

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import parallelism.Par

class ParTest extends FlatSpec
  with Matchers
  with BeforeAndAfterAll
  with GeneratorDrivenPropertyChecks {

  private val executor = Executors.newFixedThreadPool(10)

  it should "sum a list in parallel" in {
    val array = Array.fill(100)(1)
    val sum = Par.sum(array)
    Par.run(executor)(sum) should be(array.sum)
  }

  it should "calculate any max of a non empty array" in
    forAll {
      array: Array[Int] =>
        whenever(!array.isEmpty) {
          val max = Par.reduce(array, Int.MinValue)(math.max)
          Par.run(executor)(max) should be(array.max)
        }
    }

  it should "pass on internal exceptions" in {
    assertThrows[Exception](Par.run(executor)(Par.lazyUnit({throw new Exception("test"); 1})))
  }

  override def afterAll(): Unit = executor.shutdown()
}
