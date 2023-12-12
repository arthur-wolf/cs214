package recursion.logger

import recursion.interpreter
import recursion.interpreter.Operation
import recursion.interpreter.Operation.*
trait Logger:
  def log(message: String, depth: Int = 0): Unit
class LoggerBuffered extends Logger: // private var ???
  private var output = ""

  def log(message: String, depth: Int = 0): Unit =
    output = f"$output${"  " * depth}$message\n"

  def getOutput: String =
    output

object EvalLogging:
  sealed trait Expr
  case class Constant(a: Int) extends Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Sub(a: Expr, b: Expr) extends Expr

  def eval(e: Expr, l: Logger, depth: Int = 0): Int =
    e match
      case Constant(a) => 
        l.log(f"$e = $a", depth)
        a
      case Add(a, b) =>
        l.log(f"$a + $b ->", depth)
        val result = eval (a, l, depth + 1) + eval (b, l, depth + 1)
        l.log(f"= $result", depth)
        result
      case Sub(a, b) =>
        l.log(f"$a - $b ->", depth)
        val result = eval (a, l, depth + 1) - eval (b, l, depth + 1)
        l.log(f"= $result", depth)
        result
    

object InterpreterLogging:
  def evalOp(l: Logger)(stack: List[Int], op: Operation): List[Int] =
    l.log(f"$op →")
    val newStack = (op, stack) match
      case (Push(n), _)                 => n :: stack
      case (Add(), n1 :: n2 :: rest)    => (n1 + n2) :: rest
      case (Substr(), n1 :: n2 :: rest) => (n1 - n2) :: rest
      case _                            => stack

    l.log(f"Stack: ${newStack.mkString(" ")}", 1)
    newStack

  def evalRec(p: List[Operation], l: Logger): List[Int] =
    p.foldLeft(Nil)(evalOp(l))
  type Stack = scala.collection.mutable.Stack[Int]
  import scala.collection.mutable.Stack

  def evalLoop(p: List[Operation], l: Logger): Stack =
    // ---
    val stack = Stack.empty[Int]
    for op <- p do
      l.log(f"$op ->")

      op match
        case Push(n) => stack.push(n)
        case Add() if stack.size >= 2 =>
          val n1 = stack.pop()
          val n2 = stack.pop()
          stack.push(n1 + n2)
        case Substr() if stack.size >= 2 =>
          val n1 = stack.pop()
          val n2 = stack.pop()
          stack.push(n1 - n2)
        case _ => stack

      l.log(f"Stack: ${stack.mkString(" ")}")
    stack
