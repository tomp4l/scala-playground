package property

import java.util.concurrent.{ExecutorService, Executors}

import Prop._
import parallelism.Par
import parallelism.Par.Par
import pure.{RNG, SimpleRNG}
import scrictness.Stream

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (m, n, rng) => {
      run(m, n, rng) match {
        case Passed | Proved => p.run(m, n, rng)
        case x => x
      }
    }
  )

  def ||(p: Prop): Prop = Prop(
    (m, n, rng) => {
      run(m, n, rng) match {
        case Falsified(_, _) => p.run(m, n, rng)
        case x => x
      }
    }
  )
}

object Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(
           p: Prop,
           maxSize: Int = 100,
           testCases: Int = 100,
           rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop =
    Prop {
      (_, _, _) => if (p) Proved else Falsified("()", 0)
    }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf1(smallInt)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    run(maxProp)

    val sortedProp = forAll(Gen.listOf1(smallInt)) {
      ns =>
        val sorted = ns.sorted
        sorted.head == ns.min &&
          sorted.last == ns.max &&
          sorted.length == ns.length
    }

    run(sortedProp)

    val ES: ExecutorService = Executors.newCachedThreadPool

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

    val unitProp =
      check {
        val p1 = Par.map(Par.unit(1))(_ + 1)
        val p2 = Par.unit(2)
        Par.run(ES)(equal(p1, p2))
      }
    run(unitProp)

    val S = Gen
      .weighted(
        Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
        Gen.unit(Executors.newCachedThreadPool) -> .25
      )

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) { case s ** a => Par.run(s)(f(a)) }

    def checkPar(p: Par.Par[Boolean]): Prop = forAll(S)(s => Par.run(s)(p))

    val unitProp2 = checkPar {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    run(unitProp2)
  }
}