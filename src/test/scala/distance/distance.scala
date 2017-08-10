package distance

import org.scalatest.FlatSpec

class distance extends FlatSpec {

  behavior of "distance"

  it should "have a euclidean distance greater than 0" in {
    val a = Array[Double](3, 4, 5, 6, 73, 6, 7)
    val b = Array[Double](5, 6, 4, 3, 7, 8, 11)
    assert(DistanceMetrics.euclidean(a, b) > 0)
  }

  it should "have a euclidean distance equal to 0" in {
    val a = Array[Double](3, 4, 5, 6, 73, 6, 7)
    assert(DistanceMetrics.euclidean(a, a) == 0)
  }

}
