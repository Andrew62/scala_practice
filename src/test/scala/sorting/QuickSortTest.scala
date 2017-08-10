package sorting

import org.scalatest.FlatSpec
import util.Random.nextInt

class QuickSortTest extends FlatSpec {

  behavior of "QuickSortTest"

  it should "sort" in {
    val a = new Array[Int](10).map(_ => nextInt(10))
    val b = sorting.QuickSort.quick(a)
    assert(!a.zip(b).forall(x => x._1 == x._2))
    assert(a.sorted.zip(b).forall(x => x._1 == x._2))

  }

}
