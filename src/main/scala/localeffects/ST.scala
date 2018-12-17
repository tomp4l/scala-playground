package localeffects

import scala.collection.mutable

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    })
}

object Quicksort {
  private def noop[S] = ST[S, Unit](())

  private def partition[S](as: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] =
    for {
      pivotVal <- as.read(pivot)
      _ <- as.swap(pivot, r)
      j <- STRef(n)
      _ <- (n until r).foldLeft(noop[S])(
        (s, i) =>
          for {
            _ <- s
            vi <- as.read(i)
            _ <- if (vi < pivotVal) for {
              vj <- j.read
              _ <- as.swap(i, vj)
              _ <- j.write(vj + 1)
            } yield ()
            else noop[S]
          } yield ()
      )
      x <- j.read
      _ <- as.swap(x, r)
    } yield x

  private def qs[S](as: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
    if (n < r) {
      for {
        pi <- partition(as, n, r, n + (r - n) / 2)
        _ <- qs(as, n, pi - 1)
        _ <- qs(as, pi + 1, r)
      } yield ()
    } else ST(())

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
}

sealed abstract class STMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(value.size)

  def get(k: K): ST[S, Option[V]] = ST(value.get(k))

  def +=(kv: (K, V)): ST[S, Unit] = ST(value += kv)

  def -=(k: K): ST[S, Unit] = ST(value -= k)

  def freeze: ST[S, Map[K, V]] = ST(value.toMap)
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val value = mutable.HashMap.empty[K, V]
  })

  def fromMap[S, K, V](m: Map[K, V]) = ST(new STMap[S, K, V] {
    lazy val value = mutable.HashMap(m.toSeq: _*)
  })
}