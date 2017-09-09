package trees

import core.Matrix
import org.scalatest.FlatSpec
import scala.util.Random.{nextGaussian, nextInt, setSeed, nextDouble}

class DecisionTreeTest extends FlatSpec {

  behavior of "DecisionTree"
  setSeed(2019)

  val rows = 200
  val cols = 30
  val X = new Matrix[Double](rows, cols)
  val y = new Matrix[Int](rows, 1)

  // some nice Gaussian separation
  for (r <- X.rowIndices){
    for (c <- X.colIndices){
      if (r > (X.rows/2)){
        X(r)(c) = nextGaussian() + 1
        y(r)(0) = 1
      } else {
        X(r)(c) = nextGaussian()
        y(r)(0) = 0
      }
    }
  }

  "A DecisionBranch" should "be created and fit when given data" in {
    val tree = new DecisionTree(X, y, 10, 1)
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
    for (r <- y.rowIndices){
      if (y(r)(0) == y_pred(r)){
        count += 1
      }
    }
    val accuracy = count / y_pred.length.asInstanceOf[Double]
    println(s"Accuracy: $accuracy")
    assert(accuracy > 0)
  }
}
