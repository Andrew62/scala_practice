package core

import util.Random.nextDouble
import org.scalatest.FlatSpec

class MatrixTest extends FlatSpec {

  behavior of "MatrixTest"

  val a = new Matrix[Double](10, 5)
  a.fill(nextDouble)
  val b = new Matrix[Double](5, 3)
  b.fill(nextDouble)

  def f(): Double = 1
  val ones = new Matrix[Double](10, 1)
  ones.fill(f)

  it should "fill" in {
    assert(ones(0)(0) == 1)
  }

  val onesT = ones.transpose
  it should "transpose" in {
    assert(onesT.shape == (1, 10))
  }

  it should "dot" in {
    val c = MatrixMath.dot(a, b)
    assert(c.shape == Tuple2(10, 3))
    val ten = MatrixMath.dot(onesT, ones)
    assert(ten(0)(0) == 10)

    val twothree = new Matrix[Double](4, 1)
    var r = 0
    def f(): Double = {
      r += 1
      r
    }
    twothree.fill(f)
    val thirty = MatrixMath.dot(twothree.transpose, twothree)
    assert(thirty(0)(0) == 30)
  }

}
