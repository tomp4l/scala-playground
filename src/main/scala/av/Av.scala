package av

import scala.annotation.tailrec
import scala.util.Random

object Av {

  type votes = List[String]


  def main(args: Array[String]): Unit = {

    val allVotes: List[votes] =
      List.fill(5000)(List(
        "scala",
        "scala",
        "f#",
        "node",
        "php",
        "rust"
      )).map(Random.shuffle(_))

    av(allVotes)
  }

  def score(votes: List[votes]): Map[String, Int] = {
    val topVotes = votes.flatMap(_.headOption)
    val allOptions = votes.flatten.distinct
    val emptyMap = allOptions.foldLeft(Map.empty[String, Int])(
      (map, string) => map + (string -> 0)
    )
    emptyMap ++ topVotes.groupBy(a => a).mapValues(_.size)
  }

  trait Outcome

  case class Winner(winner: String, votes: Int, total: Int) extends Outcome

  case class Draw(winners: List[String], votes: Int, total: Int) extends Outcome

  case class Losers(losers: List[String]) extends Outcome

  def outcome(map: Map[String, Int]): Outcome = {
    val voteCounts = map.values
    val max = voteCounts.max
    val min = voteCounts.min
    val totalVotes = voteCounts.sum
    val winners = map.filter(_._2 == max).keys

    if (max > totalVotes / 2) {
      val winner = winners.head
      Winner(winner, max, totalVotes)
    } else {
      if (min == max) {
        Draw(winners.toList, max, totalVotes)
      } else {
        val losers = map.filter(_._2 == min).keys
        Losers(losers.toList)
      }
    }
  }

  @tailrec
  def av(votes: List[votes]): Unit = {
    outcome(score(votes)) match {
      case Winner(winner, votes, total) =>
        println(s"$winner won with $votes out of $total")
      case Draw(winners, votes, total) =>
        println(s"Draw between ${winners.mkString(", ")} with $votes each out of $total")
      case Losers(losers) =>
        println(s"Eliminating ${losers.mkString(", ")}")
        av(votes.map(_.filter(!losers.contains(_))))
    }
  }
}
