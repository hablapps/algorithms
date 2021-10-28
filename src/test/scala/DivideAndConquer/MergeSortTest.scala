package DivideAndConquer

import DivideAndConquer.DaC._
import DivideAndConquer.MergeSort._
import org.scalatest._

class MergeSortTest extends FunSuite {
  val unordered0 = Array(23, 43, 15, 32, 3, 41, 2, 2)
  val ordered0 = Array(2,2,3,15,23, 32 ,41,43)

  val unordered1 = Array(1)
  val ordered1 = Array(1)

  val unordered2 = Array(9,8,7,6,5,4,3,2,1,0)
  val ordered2 = Array(0,1,2,3,4,5,6,7,8,9)

  val unordered3 = Array(2,2,2,2,2,2)
  val ordered3 = Array(2,2,2,2,2,2)

  val unordered4 = new Array[Int](0)
  val ordered4 = new Array[Int](0)


  test("DaC"){
    assert(DaC(unordered0, decompose, merge) === ordered0)
    assert(DaC(unordered1, decompose, merge) === ordered1)
    assert(DaC(unordered2, decompose, merge) === ordered2)
    assert(DaC(unordered3, decompose, merge) === ordered3)
    assert(DaC(unordered4, decompose, merge) === ordered4)
  }
  test("DaC2 compose"){
    assert(DaC2(unordered0, decompose1,  compose) === ordered0)
    assert(DaC2(unordered1, decompose1,  compose) === ordered1)
    assert(DaC2(unordered2, decompose1,  compose) === ordered2)
    assert(DaC2(unordered3, decompose1,  compose) === ordered3)
    assert(DaC2(unordered4, decompose1,  compose) === ordered4)
  }

  test("DaC2 composeRec"){
    assert(DaC2(unordered0.toList, decompose2, composeRec1) === ordered0.toList)
    assert(DaC2(unordered1.toList, decompose2, composeRec1) === ordered1.toList)
    assert(DaC2(unordered2.toList, decompose2, composeRec1) === ordered2.toList)
    assert(DaC2(unordered3.toList, decompose2, composeRec1) === ordered3.toList)
    assert(DaC2(unordered4.toList, decompose2, composeRec1) === ordered4.toList)
  }

  test("AdHoc"){
    assert(apply(unordered0) === ordered0)
    assert(apply(unordered1) === ordered1)
    assert(apply(unordered2) === ordered2)
    assert(apply(unordered3) === ordered3)
    assert(apply(unordered4) === ordered4)
  }

}
