package example

object CubeCalculator {
  def cube(x: Int) = {
    Math.multiplyExact(Math.multiplyExact(x, x), x)
  }
}
