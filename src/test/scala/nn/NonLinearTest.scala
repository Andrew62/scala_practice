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
    assert(log_base_b(1, 4.4) == 0)
    assert(log_base_b(4, 2) == 2)
    assert(log_base_b(100, 10) == 2)
  }

  it should "tanh" in {
    assert(tanh(0) == 0)
    assert(tanh(100).round == 1)
    assert(tanh(-100).round == -1)
  }

  it should "relu" in {
    assert(relu(10) == 10)
    assert(relu(-10) == 0)
  }

  it should "leaky relu" in {
    val a = 0.001 //smaller than default
    assert(leakyRelu(10, a) == 10)
    assert(leakyRelu(-10, a) == (a * -10))
  }

}
