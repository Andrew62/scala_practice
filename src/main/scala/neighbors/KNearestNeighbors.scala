package neighbors

import core.Matrix
import distance.DistanceMetrics.euclidean

class KNearestNeighbors(val X: Matrix[Double], val y: Array[Int], val k: Int) {

  def getNeighbors(row: Array[Double]): Int = {
    val distances = this.X.container.map(x => euclidean(x, row))
    // This is where we find the nearest k neighbors and find the most
    // frequently occurring among them
    y.zip(distances)
      .sortBy(_._2)
      .slice(0, this.k)
      .map(_._1)
      .groupBy(identity)
      .mapValues(_.length)
      .maxBy(_._2)
      ._1

  }

  def predict(data: Matrix[Double]): Array[Int] = {
    data.container.map(row => getNeighbors(row))
  }
}
