package DivideAndConquer

import DivideAndConquer.DaC._
import DivideAndConquer.Power._

class PowerTest extends org.scalatest.FunSuite {
  test("DaC"){
    assert(DaC((3,3), decompose, merge)===27)
    assert(DaC((4,2), decompose, merge)===16)
    assert(DaC((5,0), decompose, merge)===1)
    assert(DaC((0,5), decompose, merge)===0)
    assert(DaC((1,5), decompose, merge)===1)
  }

  test("DaC3"){
    assert(DaC3((3,3), decompose1, compose)===27)
    assert(DaC3((4,2), decompose1, compose)===16)
    assert(DaC3((5,0), decompose1, compose)===1)
    assert(DaC3((0,5), decompose1, compose)===0)
    assert(DaC3((1,5), decompose1, compose)===1)
  }

}
