package trees

import core.Matrix
import org.scalatest.FunSuite
import scala.util.Random.{nextFloat, nextInt}

class DecisionTreeTest extends FunSuite {

  val rows = 5
  val cols = 2
  val X = new Matrix[Double](rows, cols)
  val y = new Matrix[Int](rows, 1)
  private def fillArray[A](a: Matrix[A], randFunc: () => A) = {
    for (i <- a.rowIndices) {
      for (j <- a.colIndices) {
        a(i)(j) = randFunc()
      }
    }
  }
  fillArray[Double](X, nextFloat)
  def f() = {nextInt(2)}
  fillArray[Int](y, f)

  val tree = new DecisionTree(X, y, 10, 1)
  println("done")
}
