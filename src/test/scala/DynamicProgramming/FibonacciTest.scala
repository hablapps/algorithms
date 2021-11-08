package DynamicProgramming

import org.scalatest.FunSuite
import DynamicProgramming.Fibonacci._

class FibonacciTest extends FunSuite {

  test("Fibonacci"){
    assert(apply(0)===1)
    assert(apply(1)===1)
    assert(apply(3)===3)
    assert(apply(4)===5)
    assert(apply(10)===89)
  }

}
