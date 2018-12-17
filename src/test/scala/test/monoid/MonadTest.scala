package test.monoid

import monoid.Monad
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import pure.State

class MonadTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  it should "sequence a list" in {
    val listsOfLists = List(List(1, 2), List(3, 4))
    val listOfOptions = List(Some(1), None)
    Monad.listMonad.sequence(listsOfLists) should be(List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
    Monad.optionMonad.sequence(listOfOptions) should be(None)
  }

  it should "replicate a list" in {
    val lists = List(1, 2, 3)
    Monad.listMonad.replicateM(2, lists) should be(List(List(1, 1), List(1, 2), List(1, 3), List(2, 1), List(2, 2), List(2, 3), List(3, 1), List(3, 2), List(3, 3)))
  }

  it should "hold the laws for lists" in {
    forAll {
      (f: Int => List[Int], g: Int => List[Int], h: Int => List[Int], x: Int) => {
        val laws = Monad.listMonad.Laws
        laws.associative(f, g, h, x) should be(true)
        laws.identity(f, x) should be(true)
      }
    }
  }
}
