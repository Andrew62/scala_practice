package neighbors

import core.Matrix
import org.scalatest.FlatSpec

import scala.util.Random.{nextGaussian, nextInt, setSeed}

class KNearestNeighborsTest extends FlatSpec {

  behavior of "KNearestNeighbors"
  setSeed(2017) // do this when testing random things pls

  val rows = 100
  val cols = 40
  val k = 4
  val X = new Matrix[Double](rows, cols)
  val y = new Array[Int](rows)

  // some nice Gaussian separation
  for (r <- X.rowIndices){
    for (c <- X.colIndices){
      if (r > (X.rows/2)){
        X(r)(c) = nextGaussian() + 10
        y(r) = 1
      } else {
        X(r)(c) = nextGaussian()
        y(r) = 0
      }
    }
  }

  val knn = new KNearestNeighbors(X, y, k)

  it should "predict" in {
    val pred = knn.predict(X)
    var count = pred.indices
      .map(x => if (pred(x) == y(x)) 1 else 0)
      .sum
    val accuracy = count / pred.length.asInstanceOf[Double]
    println(s"Accuracy: $accuracy")
    assert(accuracy > 0)
  }

}
