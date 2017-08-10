package sorting

import util.Random.nextInt


object QuickSort {

    def quick(arr: Array[Int]): Array[Int] = {
      if (arr.length < 2){
       return arr
      }
      val split = nextInt(arr.length)
      val pivot = arr(split)
      val left = arr.filter(_ < pivot)
      val right = arr.filter(_ > pivot)
      val center = arr.filter(_ == pivot)
      quick(left) ++ center ++ quick(right)
    }
}
