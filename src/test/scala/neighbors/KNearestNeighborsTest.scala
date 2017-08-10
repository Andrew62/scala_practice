package neighbors

import core.Matrix
import org.scalatest.FlatSpec
import scala.util.Random.{nextFloat, nextInt}

class KNearestNeighborsTest extends FlatSpec {

  behavior of "KNearestNeighbors"

  val rows = 100
  val cols = 40
  val k = 4
  val X = new Matrix[Double](rows, cols)
  val y = new Array[Int](rows).map(_ => nextInt(5))
  private def fillArray[A](a: Matrix[A], randFunc: () => A) = {
    for (i <- a.rowIndices) {
      for (j <- a.colIndices) {
        a(i)(j) = randFunc()
      }
    }
  }
  fillArray[Double](X, nextFloat)

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
