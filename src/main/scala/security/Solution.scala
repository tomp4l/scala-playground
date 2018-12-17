package security

object Solution {

  def solveCypher(key: String, message: String): Unit = {
    val chars = key
      .toList
      .distinct
    val alphabet = 'A' to 'Z'
    val remainingLetters = alphabet.filter(!chars.contains(_))

    var buckets = chars
      .map(List(_))
      .toArray
    val bucketAmount = chars.length
    var counter = 0;
    for (l <- remainingLetters) {
      val c = counter % bucketAmount
      buckets(c) = l :: buckets(c)
      counter += 1
    }
    val cypher = buckets
      .map(_.reverse)
      .sortWith((a, b) => a.head < b.head)
      .flatten

    println(message.map ({
      case ' ' => ' '
      case c => (cypher.indexOf(c) + 65).toChar
    }).mkString)
  }

  def main(args: Array[String]) {
    val cyphers = scala.io.StdIn.readInt
    for (_ <- 0 until cyphers) {
      solveCypher(
        scala.io.StdIn.readLine,
        scala.io.StdIn.readLine
      )
    }
  }
}
