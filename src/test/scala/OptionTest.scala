import datastructures._
import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers{

  it should "sequence a list of options" in {
    val someList = List(Some(1), Some(2), Some(3))
    val noneList = List(Some(1), None, Some(3))
    Option.sequence(someList) should be (Some(List(1,2,3)))
    Option.sequence(noneList) should be (None)
  }
}
