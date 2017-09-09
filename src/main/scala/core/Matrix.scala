package core

import scala.reflect.ClassTag

// TODO make this a generic number type!!!
final class Matrix[T: ClassTag](val rows: Int, val cols: Int) {
  val container: Array[Array[T]] = Array.ofDim[T](rows, cols)
  def get(row: Int, col: Int): T = container(row)(col)
  def set(row: Int, col: Int)(value: T) = { container(row)(col) = value }
  def apply(i: Int): Array[T] = { this.container(i) }
  def rowIndices: Range = { this.container.indices }
  def colIndices: Range = { this.container(0).indices }
  def distinct: Array[T] = { this.container.flatMap(_.distinct).distinct }
  def shape: Int Tuple2 Int = Tuple2(this.rows, this.cols)
  def fill(f: () => T) = {
    for (r <- this.rowIndices){
      for (c <- this.colIndices){
        this.container(r)(c) = f()
      }
    }
  }
  def transpose: Matrix[T] = {
    val newContainer = new Matrix[T](this.cols, this.rows)
    for (r <- this.rowIndices){
      for (c <- this.colIndices){
        newContainer(c)(r) = this(r)(c)
      }
    }
    newContainer
  }
}
