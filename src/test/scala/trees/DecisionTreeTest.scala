package trees

import core.Matrix
import org.scalatest.FlatSpec
import scala.util.Random.{nextFloat, nextInt}

class DecisionTreeTest extends FlatSpec {

  val rows = 100
  val cols = 30
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

  "A DecisionBranch" should "be created and fit when given data" in {
    val tree = new DecisionTree(X, y, 10, 2)
    assert(tree.root.isInstanceOf[DecisionBranch[Int]])
  }

  "Predictions" should "be made when we fit and predict" in {
    val tree = new DecisionTree(X, y, 10, 2)
    val y_pred = tree.predict(X)
    assert(y_pred.isInstanceOf[Array[Int]])
  }

  "Accuracy" should "be higher than zero" in {
    val tree = new DecisionTree(X, y, 10, 2)
    val y_pred = tree.predict(X)
    var count = 0
    for (rowidx <- 0 until y.rows) {
      if (y(rowidx)(0) == y_pred(rowidx)) {
        count += 1
      }
    }
    val accuracy = count / y_pred.length.asInstanceOf[Double]
    println(s"Accuracy: $accuracy")
    assert(accuracy > 0)
  }
}
