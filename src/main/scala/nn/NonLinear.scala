package nn

import scala.math.{E, pow, log}

object NonLinear {

  def sigmoid(x: Double): Double = 1/(1 + pow(E, -1 * x))

  def log_base_b(x: Double, base: Double): Double = log(x)/log(base)

  def inverseSigmoid(x: Double): Double = -log(1/x - 1)

}
