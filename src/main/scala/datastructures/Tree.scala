package datastructures

sealed trait Tree[+A] {
  self =>

  def size(): Int =
    self.fold[Int](_ => 1, (l, r) => 1 + l + r)

  def maximum[B >: A]()(implicit cmp: Ordering[B]): A =
    self.fold[A](v => v, (l, r) => if (cmp.gt(l, r)) l else r)

  def depth(): Int =
    self.fold[Int](_ => 1, (l, r) => 1 + math.max(l, r))

  def map[B >: A](f: A => B): Tree[B] =
    self.fold[Tree[B]](v => Leaf(f(v)), (l, r) => Node(l, r))

  def fold[B](leafMap: A => B, nodeMap: (B, B) => B): B = {
    def loop(node: Tree[A]): B =
      node match {
        case Leaf(value) => leafMap(value)
        case Node(left, right) => nodeMap(loop(left), loop(right))
      }

    loop(self)
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]