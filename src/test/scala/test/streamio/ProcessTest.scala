package test.streamio

import org.scalatest.{FlatSpec, Matchers}
import streamio.Process

class ProcessTest extends FlatSpec with Matchers {
  it should "sum a stream" in {
    Process.sum(Stream(1.0, 2.0, 3.0, 4.0)).toList should be(List(1.0, 3.0, 6.0, 10.0))
  }

  it should "count a stream" in {
    Process.count(Stream(3, 2, 5, 1)).toList should be(List(1, 2, 3, 4))
  }


  it should "take from a stream in" in {
    Process.take(2)(Stream(1, 2, 3, 4)).toList should be(List(1, 2))
  }

  it should "drop from a stream in" in {
    Process.drop(2)(Stream(1, 2, 3, 4)).toList should be(List(3, 4))
  }

  it should "calculate a running mean" in {
    Process.mean(Stream(1.0, 4.0, 4.0, 3.0)).toList should be(List(1.0, 2.5, 3.0, 3.0))
  }

  it should "zip with index" in {
    Process
      .id
      .zipWithIndex(Stream(1, 2, 3, 4))
      .toList should be(List((1, 0), (2, 1), (3, 2), (4, 3)))
  }

  it should "zip two streams" in {
    Process
      .id
      .zip(Process.sum)(Stream(1, 2, 3, 4)) should be(List((1, 1), (2, 3), (3, 6), (4, 10)))
  }
}
