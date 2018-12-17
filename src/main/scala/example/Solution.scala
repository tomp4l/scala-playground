package example

import java.io.PrintWriter

import scala.annotation.tailrec
import scala.io.StdIn

object Solution {

  // Complete the divisibleSumPairs function below.
  def divisibleSumPairs(n: Int, k: Int, ar: Array[Int]): Int = {
    countPairs(0, k, ar)
  }

  @tailrec
  def countPairs(acc: Int, k: Int, ar: Array[Int]): Int = {
    if (ar.length < 2) {
      acc
    } else {
      val head = ar.head
      countPairs(acc + ar.tail.count(i => (i + head) % k == 0), k, ar.tail)
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    //      val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nk = stdin.readLine.split(" ")

    val n = nk(0).trim.toInt

    val k = nk(1).trim.toInt

    val ar = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = divisibleSumPairs(n, k, ar)
    val l : List[Int] = Nil;
    println(result)

  }
}