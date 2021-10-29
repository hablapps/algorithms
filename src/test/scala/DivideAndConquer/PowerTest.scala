package DivideAndConquer

import DivideAndConquer.Power._

class PowerTest extends org.scalatest.FunSuite {

  test("Power"){
    assert(apply(3,3)===27)
    assert(apply(4,2)===16)
    assert(apply(5,0)===1)
    assert(apply(0,5)===0)
    assert(apply(1,5)===1)
  }

}
