package nn

import scala.math.{E, pow, log}

object NonLinear {

  def sigmoid(x: Double): Double = 1/(1 + pow(E, -1 * x))

  def log_base_b(x: Double, base: Double): Double = log(x)/log(base)

  def inverseSigmoid(x: Double): Double = -log(1/x - 1)

  def tanh(x: Double) : Double = (pow(E, x) - pow(E, -1 * x)) / (pow(E, x) + pow(E, -1 * x))

  def relu(x: Double): Double = if (x >= 0) x else 0 // notice this includes zero. relu is undefined at zero

  /**
    * Implementation of leaky relu where partial gradients
    * are passed via a * x if x < 0. We include zero even though
    * it is strictly undefined b/c according to Andrew Ng the chances
    * are small that neural net params will be zero
    */
  def leakyRelu(x: Double, a: Double = 0.01): Double = if (x >= 0) x else a * x

}
