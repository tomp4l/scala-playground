import datastructures.{Leaf, Node}
import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  it should "count the nodes in a tree" in {
    val tree = Node(Leaf(1), Leaf(2))
    tree.size() should be (3)
  }

  it should "find the maximum of the tree" in {
    val tree = Node(Leaf(1), Node(Leaf(4), Leaf(2)))
    tree.maximum() should be (4)
  }

  it should "find the depth of the tree" in {
    val tree = Node(Leaf(1), Node(Node(Leaf(3), Leaf(4)), Leaf(2)))
    tree.depth() should be (4)
  }

  it should "map all values in a tree" in {
    val tree = Node(Leaf(1), Node(Leaf(2), Leaf(3)))
    tree.map(_ + 1) should be (Node(Leaf(2), Node(Leaf(3), Leaf(4))))
  }
}