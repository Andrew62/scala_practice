package trees

sealed trait Tree[+T]
final case class Leaf[T](value: T) extends Tree[T]
final case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
final case class DecisionBranch[T](left: Option[DecisionBranch[T]] = None,
                      right: Option[DecisionBranch[T]] = None,
                      splitValue: Option[Double] = None,
                      colIndex: Option[Int] =  None,
                      classValue: Option[Int] = None) extends Tree[T]