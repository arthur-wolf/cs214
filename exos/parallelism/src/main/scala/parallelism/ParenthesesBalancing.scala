package parallelism

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.mutable.ParArray

object ParenthesesBalancing:
  // start isBalancedRecursive
  def isBalancedRecursive(str: List[Char]): Boolean =
    def loop(chars: List[Char], open: Int): Boolean =
      if open < 0 then false
      else
        chars match
          case Nil => open == 0
          case '(' :: tail => loop(tail, open + 1)
          case ')' :: tail => loop(tail, open - 1)
          case _ :: tail => loop(tail, open)

    loop(str, 0)
  // end isBalancedRecursive

  // start isBalancedFold
  def isBalancedFold(str: List[Char]): Boolean =
    val foldingFunction: (Int, Char) => Int =
      (count, char) =>
        char match
          case '(' => count + 1
          case ')' => count - 1
          case _ => count

    str.foldLeft(0)(foldingFunction) == 0
  // end isBalancedFold

  // start isBalancedParSimple
  def isBalancedParSimple(str: List[Char]): Boolean =
    val foldingFunction: (Int, Char) => Int =  // your folding function
      (count, char) =>
        char match
          case '(' => count + 1
          case ')' => count - 1
          case _ => count

    val numOpen = str.par.aggregate(0)(foldingFunction, _ + _)

    (numOpen == 0)
  // end isBalancedParSimple

  // start isBalancedPar
  def isBalancedPar(str: List[Char]): Boolean =
    val seqOp: (Any, Char) => Any = 
      (count, char) =>
        char match
          case '(' => count.asInstanceOf[Int] + 1
          case ')' => count.asInstanceOf[Int] - 1
          case _ => count
    val combOp: (Any, Any) => Any = 
      (count1, count2) =>
        (count1.asInstanceOf[Int] + count2.asInstanceOf[Int]).asInstanceOf[Any]

    str.aggregate(???)(seqOp, combOp) == 0
  // end isBalancedPar
