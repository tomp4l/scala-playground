import example.MyModule
import org.scalatest.{FlatSpec, Matchers}

class MyModuleTest extends FlatSpec with Matchers {

  it should "calculate the nth fibonacci number" in {
    MyModule.fibonacci(0) should be(0)
    MyModule.fibonacci(1) should be(1)
    MyModule.fibonacci(2) should be(1)
    MyModule.fibonacci(3) should be(2)
    MyModule.fibonacci(4) should be(3)
    MyModule.fibonacci(5) should be(5)
  }

  it should "check if a list is ordered" in {
    def ord(a: Int, b: Int): Boolean = a >= b;

    MyModule.isSorted(Array(), ord) should be(true)
    MyModule.isSorted(Array(1), ord) should be(true)
    MyModule.isSorted(Array(1, 2, 3), ord) should be(true)
    MyModule.isSorted(Array(1, 1, 1), ord) should be(true)
    MyModule.isSorted(Array(1, 3, 1), ord) should be(false)
  }

  it should "curry and uncurry" in {
    def add(a: Int, b: Int): Int = a + b

    def addC(a: Int)(b: Int): Int = a + b

    MyModule.curry(add)(1)(2) should be(3)
    MyModule.uncurry(addC)(1, 2) should be(3)
  }

  it should "compose functions" in {
    def stringInt(a: Int): String = a.toString

    def double(a: Int): Int = a * 2

    MyModule.compose(stringInt, double)(2) should be("4")

    val x = List(1);
  }
}