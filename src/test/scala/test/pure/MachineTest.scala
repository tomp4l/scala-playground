package test.pure

import org.scalatest.{FlatSpec, Matchers}
import pure.{Coin, Machine, Turn}

class MachineTest extends FlatSpec with Matchers {

  it should "dispense a candy for a coin" in {
    Machine.simulateMachine(List(Coin, Turn)).run(Machine(locked = true, 1, 0))._1 should
      be(1, 0)
  }

  it should "dispense many candies" in {
    Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 10, 0))._1 should be(4, 6)
  }

  it should "should only take one coin" in {
    Machine.simulateMachine(List(Coin, Coin, Coin, Coin, Coin, Coin, Coin))
      .run(Machine(locked = true, 10, 0))._1 should be(1, 10)
  }

  it should "will not take a coin without candies" in {
    Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
      .run(Machine(locked = true, 0, 0))._1 should be(0, 0)
  }
}
