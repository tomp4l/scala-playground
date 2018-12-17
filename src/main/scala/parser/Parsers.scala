package parser

import property.{Gen, Prop, SGen}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex
import Parsers._
import property.Prop.forAll


trait Parsers[Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => p2.map(a -> _))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) {
      map2(p, listOfN(n - 1, p))(_ :: _)
    } else {
      succeed(List.empty[A])
    }

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(Nil: List[A])

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => p2.map(f(a, _)))

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    flatMap(slice(p))(_ => p2)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def as[A, B](p: Parser[A], b: B): Parser[B] =
    map(slice(p))(_ => b)

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def skipL[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def skipR[B](p2: => Parser[B]): Parser[A] = self.skipR(p, p2)

    def as[B](b: B): Parser[B] = self.as(p, b)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))

    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop = {
      def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

      def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in)
    }

    def mapProductLaw[A, B, C, D](a: Parser[A], b: Parser[B], f: A => C, g: B => D)(in: Gen[String]): Prop =
      equal(
        a.map(f) ** b.map(g),
        (a ** b) map { case (aa, bb) => (f(aa), g(bb)) }
      )(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
  }

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String
}


object Parsers {

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latestLoc: Option[Location] = latest map (_._1)

    def latest: Option[(Location, String)] = stack.lastOption
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError = ParseError(List(this -> msg))
  }

  type Parser[+A] = Location => Result[A]

}


object MyParsers extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0)) match {
      case Parsers.Success(get, _) => Right(get)
      case Parsers.Failure(get) => Left(get)
    }

  override implicit def string(s: String): Parser[String] =
    scope("string") {
      l: Location =>
        if (l.input.startsWith(s))
          Success(s, s.length)
        else
          Failure(l.toError("Expected: " + s))
    }


  override implicit def regex(r: Regex): Parser[String] =
    scope("regex") {
      l: Location =>
        r.findPrefixOf(l.input) match {
          case Some(s) => Success(s, s.length)
          case None => Failure(l.toError("Expected: " + r.toString()))
        }
    }

  override def slice[A](p: Parser[A]): Parser[String] =
    (l: Location) =>
      p(l) match {
        case Success(_, charsConsumed) =>
          Success(l.input.slice(0, charsConsumed), charsConsumed)
        case Failure(e) =>
          Failure(e)
      }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    (l: Location) =>
      s1(l) match {
        case Parsers.Success(get, charsConsumed) =>
          Parsers.Success(get, charsConsumed)
        case Parsers.Failure(_) =>
          s2(l)
      }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    (l: Location) =>
      p(l) match {
        case Parsers.Success(get, charsConsumed) =>
          val l2 = Location(l.input.substring(charsConsumed), l.offset + charsConsumed)
          f(get)(l2) match {
            case Parsers.Success(get2, charsConsumed2) =>
              Success(get2, charsConsumed + charsConsumed2)
            case Parsers.Failure(get2) => Parsers.Failure(get2)
          }
        case Parsers.Failure(get) => Parsers.Failure(get)
      }

  override def succeed[A](a: A): Parser[A] =
    (_: Location) => Success(a, 0)

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???

}


