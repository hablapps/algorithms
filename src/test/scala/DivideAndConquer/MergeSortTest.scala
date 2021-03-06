package DivideAndConquer

import DivideAndConquer.MergeSort._
import org.scalatest._

class MergeSortTest extends FunSuite {
  val unordered0 = Array(23, 43, 15, 32, 3, 41, 2, 2)
  val ordered0 = Array(2, 2, 3, 15, 23, 32, 41, 43)

  val unordered1 = Array(1)
  val ordered1 = Array(1)

  val unordered2 = Array(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  val ordered2 = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  val unordered3 = Array(2, 2, 2, 2, 2, 2)
  val ordered3 = Array(2, 2, 2, 2, 2, 2)

  val unordered4 = new Array[Int](0)
  val ordered4 = new Array[Int](0)

  test("MergeSort") {
    assert(apply(unordered0) === ordered0)
    assert(apply(unordered1) === ordered1)
    assert(apply(unordered2) === ordered2)
    assert(apply(unordered3) === ordered3)
    assert(apply(unordered4) === ordered4)
  }
}
