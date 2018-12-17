package genes

import java.util.StringTokenizer
import scala.annotation.tailrec
import util.control.Breaks._

object Solution {

  def buildTree(genes: Array[String], healths: Array[Int]): Trie = {
    val trie = new Trie
    var i = 0
    while (i < genes.length) {
      trie.add(genes(i), i, healths(i))
      i = i + 1
    }
    trie
  }

  def scoreDna(first: Int, last: Int, dna: String, trie: Trie): Long = {
    var sum = 0L
    for (i <- dna.indices) {
      sum = sum + trie.score(first, last, i, dna)
    }
    sum
  }

  def main(args: Array[String]) {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val stdin = new BufferedReader(new InputStreamReader(System.in), 1 << 16)

    val numberGenes = stdin.readLine.trim.toInt
    val genes = Array.ofDim[String](numberGenes)
    val geneTokenizer = new StringTokenizer(stdin.readLine)
    var i = 0
    while (i < numberGenes) {
      genes(i) = geneTokenizer.nextToken()
      i = i + 1
    }

    val health = Array.ofDim[Int](numberGenes)
    val healthTokenizer = new StringTokenizer(stdin.readLine)
    i = 0
    while (i < numberGenes) {
      health(i) = healthTokenizer.nextToken().toInt
      i = i + 1
    }

    val tree = buildTree(genes, health)
    val s = stdin.readLine.trim.toInt

    var min: Option[Long] = None
    var max: Option[Long] = None

    for (_ <- 1 to s) {
      val firstLastD = stdin.readLine.split(" ")

      val first = firstLastD(0).trim.toInt

      val last = firstLastD(1).trim.toInt

      val d = firstLastD(2)

      val score = scoreDna(first, last, d, tree)

      min = Some(min.map(_.min(score)).getOrElse(score))
      max = Some(max.map(_.max(score)).getOrElse(score))
    }

    println(f"${min.getOrElse(0)} ${max.getOrElse(0)}")
  }

  class Trie {

    class Node {
      private val children: Array[Node] = Array.ofDim(26)
      private var indices: Vector[(Int, Long)] = Vector.empty

      def create(c: Char): Node = {
        val node = new Node
        children(c - 'a') = node
        node
      }

      def get(c: Char): Node = children(c - 'a')

      def addHealth(index: Int, health: Int): Unit = {
        val (_, lastHealth) = indices.lastOption.getOrElse(0 -> 0L)
        indices = indices :+ (index -> (lastHealth + health))
      }

      def score(first: Int, last: Int): Long = {
        val indexLength = indices.length
        var sum = 0L
        if (indexLength == 0) {
          0L
        } else {
          val start = findIndex(0, indexLength - 1, first - 1)
          val end = findIndex(0, indexLength - 1, last)
          if (indices(start)._1 < first) {
            sum = sum - indices(start)._2
          }
          if (indices(end)._1 <= last) {
            sum = sum + indices(end)._2
          }
          sum
        }
      }

      @tailrec
      private def findIndex(start: Int, end: Int, target: Int): Int = {
        val index = ((end - start) / 2) + start
        if (indices(index)._1 == target) {
          index
        } else if (indices(start)._1 == target) {
          start
        } else if (indices(end)._1 == target) {
          end
        } else {
          val (newStart, newEnd) = if (indices(index)._1 > target) {
            (start, index)
          } else {
            (index, end)
          }
          if (newEnd - newStart <= 1) {
            if (indices(newEnd)._1 < target) {
              newEnd
            } else {
              newStart
            }
          } else {
            findIndex(newStart, newEnd, target)
          }
        }
      }
    }

    var root = new Node

    def add(word: String, index: Int, health: Int): Unit = {
      val chars = word.toCharArray
      var endNode = root
      for (i <- chars.indices) {
        val c = chars(i)
        endNode = endNode.get(c) match {
          case null => endNode.create(c)
          case n => n
        }
      }
      endNode.addHealth(index, health)
    }

    def score(first: Int, last: Int, index: Int, dna: String): Long = {
      var currentIndex = index
      var sum = 0L
      var node: Node = root
      val length = dna.length
      breakable {
        do {
          node = node.get(dna(currentIndex))
          if (node == null) {
            break
          }
          sum = sum + node.score(first, last)
          currentIndex = currentIndex + 1
        } while (currentIndex < length)
      }
      sum
    }
  }
}
