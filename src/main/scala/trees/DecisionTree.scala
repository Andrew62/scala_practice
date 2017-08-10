package trees

import scala.collection.mutable.ListBuffer
import core.Matrix


// container for binary splits
case class SplitContainer(X: Matrix[Double], y: Matrix[Int])

// For passing split info between functions
case class SplitInfo(index: Int, value: Double, splits: List[SplitContainer])


class DecisionTree(val X: Matrix[Double], val y: Matrix[Int], val maxDepth: Int, val minLeafSize: Int) {

  private val uniqueClasses = y.distinct

  /** Calculate the Gini Impurity given a split of data
    * The Gini impurity is calculated at sum(p_i * (1-p_i)) where
    * p_i is the probability of being in the class which is implemented
    * here as being the proportion of the splits
   */
  def giniImpurity(splits: List[SplitContainer]): Double = {
    var gini = 0.0
    for (class_value <- this.uniqueClasses){
      for (split <- splits){
        if (split.y.rows > 0){
          val count = split.y.container.flatMap(_.filter(_ == class_value)).length
          val proportion = count / split.y.rows.toDouble
          gini += (proportion * (1.0 - proportion))
        }
      }
    }
    gini
  }

  /**
    * Returns a SplitContainer that just holds the X and y values for the
    * split specified by the rowIndex
   */
  def splitSide(rowIndex: ListBuffer[Int], dataset: Matrix[Double], targets: Matrix[Int]) : SplitContainer ={
    val outX = new Matrix[Double](rowIndex.size, dataset.cols)
    val outY = new Matrix[Int](rowIndex.size, 1)
    for (idx <- rowIndex.indices){
      outY(idx)(0) = rowIndex(idx)
      for (col <- outX.colIndices){
        outX(idx)(col) = dataset(idx)(col)
      }
    }
    SplitContainer(outX, outY)
  }

  def testSplit(dataset: Matrix[Double], targets: Matrix[Int], colIndex: Int, splitValue: Double): List[SplitContainer] = {
    val left = new ListBuffer[Int]()
    val right = new ListBuffer[Int]()

    // Go through each row
    for (rowIdx <- dataset.rowIndices){
      // we're looking to split on a specific col index and collect splits
      // into two groups
      if (dataset(rowIdx)(colIndex) < splitValue) {
        left += rowIdx
      } else {
        right += rowIdx
      }
    }
    // Copying the relevant data from the current dataset
    // over for use in discrediting the tree further
    List[SplitContainer](splitSide(left, dataset, targets), splitSide(right, dataset, targets))
  }

  def getSplit(dataset: Matrix[Double], targets: Matrix[Int], depth: Int): DecisionBranch[Int] = {
    // if we hit our limit in terms of depth or leaf size it's time to terminate
    if (depth >= this.maxDepth || targets.rows <= this.minLeafSize || targets.rows <= 1){
      return toTerminal(targets)
    }
    var bestIndex: Option[Int]= Option[Int](-1)
    var bestValue: Option[Double] = Option[Double](-1)
    var bestScore: Double = 2.0 // Gini is from 0 to 1
    var bestSplits: List[SplitContainer] = List[SplitContainer]()
    for (rowIdx <- 0 until dataset.rows) {
      for (colIdx <- 0 until dataset.cols) {
        // basically grid search here to find a col and value to split on
        val splitValue = dataset(rowIdx)(colIdx)
        val splits = this.testSplit(dataset, targets, colIdx, splitValue)
        val gini = this.giniImpurity(splits)
        if (gini < bestScore) {
          bestIndex = Option[Int](colIdx)
          bestValue = Option[Double](splitValue)
          bestScore = gini
          bestSplits = splits
        }
      }
    }
    DecisionBranch[Int](Some(this.getSplit(bestSplits.head.X, bestSplits.head.y, depth + 1)),
      Some(this.getSplit(bestSplits(1).X, bestSplits(1).y, depth + 1)), bestValue, bestIndex)
  }

  def toTerminal(targets: Matrix[Int]): DecisionBranch[Int] = {
    // This finds the most frequently occurring value in a
    // matrix of integers
    val classLabel = targets
      .container
      .flatMap(_.groupBy(identity)
        .mapValues(_.length))
      .groupBy(_._1)
      .mapValues(_.map(_._2)
        .sum)
      .maxBy(_._2)
      ._1

    DecisionBranch[Int](classValue = Option[Int](classLabel))
  }


  val root: DecisionBranch[Int] = getSplit(this.X, this.y, 0)

  def predict(data: Matrix[Double]) : Array[Int] = {
      data.container.map(x => realPredict(x, this.root, this.root.colIndex, this.root.splitValue))
    }

    /**
      * This is the real function we'll use to predict and just
      * provide an interface through predict() above
      */
    private def realPredict(data: Array[Double],
                            tree: DecisionBranch[Int],
                            colIdx: Option[Int],
                            splitValue: Option[Double]) : Int = {
      if (tree.classValue.nonEmpty) {
        tree.classValue.getOrElse(-1)
      } else if (data(colIdx.getOrElse(-1: Int)) < splitValue.getOrElse(-1: Double)){
        realPredict(data, tree.left.getOrElse(DecisionBranch[Int]()), tree.colIndex, tree.splitValue)
      } else {
        realPredict(data, tree.right.getOrElse(DecisionBranch[Int]()), tree.colIndex, tree.splitValue)
      }
    }

}
