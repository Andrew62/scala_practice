package trees

import com.sun.source.tree.BinaryTree

//sealed trait Tree[+T]
//final case class Leaf[T](value: T) extends Tree[T]
//final case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
//final case class DBranch[T](left: Tree[T],
//                      right: Tree[T],
//                      splitValue: Double,
//                      colIndex: Int) extends Tree[T]