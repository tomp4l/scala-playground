package stuff

import java.io._

import scala.collection.generic.IsTraversableLike

object Solution {

  val symbols = Array(
    ('I', 'V'),
    ('X', 'L'),
    ('C', 'D'),
    ('M', ' ')
  )

  val symbolValues = Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

  // Complete the breakingRecords function below.
  def toRoman(number: Int): String = {
    number
      .toString
      .toList
      .reverse
      .zipWithIndex
      .map {
        case (d, i) =>
          val (one, five) = symbols(i)
          d match {
            case '0' => ""
            case '1' => f"$one"
            case '2' => f"$one$one"
            case '3' => f"$one$one$one"
            case '4' => f"$one$five"
            case '5' => f"$five"
            case '6' => f"$five$one"
            case '7' => f"$five$one$one"
            case '8' => f"$five$one$one$one"
            case '9' =>
              val (ten, _) = symbols(i + 1)
              f"$one$ten"
          }
      }
      .reverse
      .mkString
  }

  def fromRoman(numeral: String): Int = {
    numeral
      .toList
      .foldLeft((None: Option[Int], 0))(
        (acc, char) => acc match {
          case (None, v) => (Some(symbolValues(char)), v)
          case (Some(carry), v) =>
            val value = symbolValues(char)
            if (value > carry) {
              (None, v + value - carry)
            } else {
              (Some(value), v + carry)
            }
        }
      ) match {
      case (None, value) => value
      case (Some(carry), value) => carry + value
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    //    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    //    val result = toRoman(stdin.readLine.trim.toInt)
    val result = fromRoman(stdin.readLine)
    println(result)
    //    printWriter.close()
  }
}