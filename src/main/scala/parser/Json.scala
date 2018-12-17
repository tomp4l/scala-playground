package parser

import parser.Json.jsonParser

import scala.language.higherKinds

trait Json


object Json {

  case object JNull extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case class JBool(get: Boolean) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JObject(get: Map[String, Json]) extends Json

  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    val whiteSpace = (char(' ') | char('\n')).many.slice

    def escapedString = "\"([^\"]|\\\")*?\"".r.map(
      _.stripPrefix("\"").stripSuffix("\"")
        .replaceAll("\\\"", "\"")
    )

    def double =
      "[0-9]+(\\.[0-9]+)?(e[0-9]+)?".r.map(_.toDouble)

    def literal: Parser[Json] = {
      "null".as(JNull) |
        "true".as(JBool(true)) |
        "false".as(JBool(false)) |
        double.map(JNumber) |
        escapedString.map(JString)
    }

    def array: Parser[Json] =
      (char('[') skipL whiteSpace skipL char(']') as JArray(Vector.empty)) |
        char('[') skipL
        ((jsonValue skipR char(',')).many ** jsonValue)
          .map { case (heads, tail) =>
            JArray(heads.toVector :+ tail)
          } skipR char(']')

    def keyPair =
      (whiteSpace skipL escapedString skipR whiteSpace) **
        (char(':') skipL jsonValue)

    def jObject: Parser[Json] =
      (char('{') skipL whiteSpace skipL char('}') as JObject(Map.empty)) |
        ((char('{') skipL
        ((keyPair skipR char(',')).many ** keyPair)
          .map { case (heads, tail) =>
            JObject((heads.toVector :+ tail).toMap)
          }) skipR char('}'))


    def jsonValue: Parser[Json] = whiteSpace skipL (literal | array | jObject) skipR whiteSpace

    jsonValue
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val json2 ="{\n  \"Company name\" : \"Microsoft Corporation\",\n  \"Ticker\"  : \"MSFT\",\n       \"Active\"  : true,\n          \"Price\"   : 30.66,\n          \"Shares outstanding\" : 8.38e9,\n          \"Related companies\" :\n            [ \"HPQ\", \"IBM\", \"YHOO\", \"DELL\", \"GOOG\" ]\n        }"

    println(json2)
    println(MyParsers.run(jsonParser(MyParsers))(json2))
  }
}