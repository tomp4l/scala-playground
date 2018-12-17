package pure

import State._

object State {

  def unit[A, S](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => {
      fs.foldLeft((List.empty[A], s))((acc, state) => {
        val (l, s1) = acc
        val (a, s2) = state.run(s1)
        (a :: l, s2)
      })
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s2) = this.run(s)
        g(a).run(s2)
      }
    )

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- rb
    } yield f(a, b)

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def machineModifier(input: Input): Machine => Machine = (machine: Machine) =>
    (input, machine) match {
      case (_, Machine(_, candies, _)) if candies == 0 => machine
      case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
      case _ => machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(i => modify(machineModifier(i))))
      s <- get
    } yield (s.coins, s.candies)
}