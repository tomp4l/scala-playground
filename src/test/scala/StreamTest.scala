import org.scalatest.{FlatSpec, Matchers}
import scrictness._

class StreamTest extends FlatSpec with Matchers {

  it should "take an amount of items" in {
    Stream(1, 2, 3).take(2).toList should be(Stream(1, 2).toList)
  }

  it should "drop an amount of items" in {
    Stream(1, 2, 3).drop(2).toList should be(Stream(3).toList)
  }

  it should "take while less than 3" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList should be(Stream(1, 2).toList)
  }

  it should "be able to get the head as an option" in {
    Stream(1, 2).headOption should be(Some(1))
    Stream.empty.headOption should be(None)
  }

  it should "work incrementally" in {
    var counter = 0
    Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      .map(a => {
        counter = counter + 1;
        a
      })
      .filter(_ => {
        counter = counter + 1;
        true
      })
      .headOption
    counter should be(2)
  }

  it should "work with infinite streams" in {
    Stream.ones.take(5).toList should be(List(1, 1, 1, 1, 1))
  }

  it should "generate a constant infinite list" in {
    Stream.constant(1).take(5).toList should be(List(1, 1, 1, 1, 1))
  }

  it should "generate a sequence of integers" in {
    Stream.from(1).take(5).toList should be(List(1, 2, 3, 4, 5))
  }

  it should "generate fibonacci numbers" in {
    Stream.fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "unfold a list of integers" in {
    Stream.unfold(0)(i => Some((i + 1, i + 1))).take(5).toList should be(List(1, 2, 3, 4, 5))
  }

  it should "map the stream" in {
    Stream(1, 2, 3, 4).map(_ + 1).toList should be(List(2, 3, 4, 5))
  }

  it should "zip 2 stream" in {
    Stream.ones.zipWith(Stream.ones)(_ + _).take(3).toList should be(List(2, 2, 2))
  }

  it should "match streams that start the same" in {
    val test = Stream(1, 2, 3, 4, 5)
    test.startsWith(Stream(1)) should be(true)
    test.startsWith(Stream.empty) should be(true)
    test.startsWith(test) should be(true)
    test.startsWith(Stream(1, 2, 4)) should be(false)
    Stream.ones.startsWith(Stream(1, 1, 1, 1)) should be(true)
  }

  it should "generate tails" in {
    Stream(1, 2, 3)
      .tails
      .map(_.toList)
      .toList should be(List(List(1, 2, 3), List(2, 3), List(3)))
  }

  it should "find a subsequence" in {
    val test = Stream(1, 2, 3, 4, 5)
    test.hasSubsequence(Empty) should be(true)
    test.hasSubsequence(Stream(2, 3, 4)) should be(true)
    test.hasSubsequence(Stream(5)) should be(true)
    test.hasSubsequence(Stream(5, 4)) should be(false)
  }

  it should "scan right through the stream" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }
}
