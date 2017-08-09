package core

import org.scalatest.FunSuite
import util.Random.nextDouble

class MatrixTest extends FunSuite {

  val sqaure: Int = 10
  val mat = new Matrix[Double](sqaure, sqaure)
  for (r <- 0 to sqaure){
    for (c <- 0 to sqaure){
      mat(r)(c) = nextDouble()
    }
  }

}
