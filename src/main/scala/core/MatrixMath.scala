package core


object MatrixMath {

  // TODO make this generic number type. How is this done in Scala?
  def dot(a: Matrix[Double], b: Matrix[Double]): Matrix[Double] = {
    if (a.cols != b.rows) {
      throw new RuntimeException(s"Shape mismatch: (${a.rows}, ${a.cols}) (${b.rows}, ${b.cols})")
    }
    val outMatrix = new Matrix[Double](a.rows, b.cols)
    for (ridx <- a.rowIndices) {
      for (cidx <- a.colIndices) {
          for (oCol <- b.colIndices) {
            outMatrix(ridx)(oCol) += a.container(ridx)(cidx) * b.container(cidx)(oCol)
          }
        }
      }
    outMatrix
  }
}
