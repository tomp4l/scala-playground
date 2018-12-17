import datastructures._
import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {


  it should "drop a number off the head" in {
    val l = List(1, 2, 3, 4, 5)
    List.drop(l, 0) should be(l)
    List.drop(l, 1) should be(List(2, 3, 4, 5))
    List.drop(l, 5) should be(List())
  }


  it should "drop a number while something is true" in {
    val l = List(1, 2, 3, 4, 5)
    List.dropWhile(l)(_ < 3) should be(List(3, 4, 5))
  }

  it should "return all but the last of a list" in {
    List.init(List(1, 2, 3, 4)) should be(List(1, 2, 3))
  }


  it should "fold a list" in {
    val l = List(1, 2, 3, 4, 5)

    List.foldLeft(l, 0)(_ + _) should be(15)
    List.foldLeft(l, 1)(_ * _) should be(120)
    List.foldLeft(l, 0)((a, _) => a + 1) should be(5)
  }

  it should "reverse a list" in {
    List.rev(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  it should "right fold list" in {
    List.foldLeft(List(1, 2, 3), 1)((a, b) => a - b) should be(-5)
    List.foldRight(List(1, 2, 3), 1)(_ - _) should be(1)
    List.foldRightUsingLeft(List(1, 2, 3), 1)(_ - _) should be(1)
  }

  it should "append to a list" in {
    List.append(List(), List(1)) should be(List(1))
    List.append(List(1, 2), List(3)) should be(List(1, 2, 3))
  }

  it should "flatten lists" in {
    List.flatten(List(List(1, 2), List(3, 4), List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
    List.flattenNotTailRec(List(List(1, 2), List(3, 4), List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  it should "add 1 to a list" in {
    List.add(List(1, 2, 3), 1) should be(List(2, 3, 4))
  }

  it should "convert to string" in {
    List.toString(List(1.0, 2.0, 3.5)) should be(List("1.0", "2.0", "3.5"))
  }

  it should "filter the odds" in {
    List.filter(List(1, 2, 3,4))(_ % 2 == 0) should be(List(2,4))
  }

  it should "flatly map" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  it should "filter the odds even more" in {
    List.filter2(List(1, 2, 3,4))(_ % 2 == 0) should be(List(2,4))
  }

  it should "add two lists" in {
    List.addTwo(List(1,2,3), List(4,5,6)) should be(List(5,7,9))
  }

  it should "match a subsequence" in {
    List.hasSubsequence(List(1,2,3), List(2,3)) should be(true)
    List.hasSubsequence(List(1,2,3), List(3,2)) should be(false)
  }
}
