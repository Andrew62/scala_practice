package nn

import NonLinear._
import org.scalatest.FlatSpec

class NonLinearTest extends FlatSpec {

  behavior of "NonLinear"

  it should "inverseSigmoid" in {
    val x: Long = 10
    assert(x == inverseSigmoid(sigmoid(x)).round)

  }

  it should "log_base_b" in {
    assert(log_base_b(10, 10) == 1)
    assert(log_base_b(5.5, 5.5) == 1) // for good measure :)
  }

}
