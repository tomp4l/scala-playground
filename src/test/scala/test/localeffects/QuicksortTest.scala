package test.localeffects

import localeffects.Quicksort
import org.scalatest.{FlatSpec, Matchers}

class QuicksortTest extends FlatSpec with Matchers{

  it should "sort lists" in {
    val list = List (2, 4, 6, 1, 3, 5)
    Quicksort.quicksort(list) should be (list.sorted)
  }
}
