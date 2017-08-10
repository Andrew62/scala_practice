package distance

object DistanceMetrics {
  def euclidean(a: Array[Double], b: Array[Double]): Double = {
    scala.math.sqrt(a.zip(b).map(x => scala.math.pow(x._1 - x._2, 2)).sum)
  }
}
