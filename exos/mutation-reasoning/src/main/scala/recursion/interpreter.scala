package recursion.interpreter
enum Operation:
  case Push(n: Int)
  case Add()
  case Substr()

type Program = List[Operation]

object RecursiveInterpreter:
  import Operation.*
  type Stack = List[Int]
  def evalOp(stack: Stack, op: Operation): Stack =
    (op, stack) match
      case (Push(n), _) => n :: stack
      case (Add(), a :: b :: rest) =>  (a + b) :: rest
      case (Substr(), a :: b :: rest) =>  (a + b) :: rest
      case _ => stack
    
  def eval(p: List[Operation]): Stack =
    p.foldLeft(Nil)(evalOp)

end RecursiveInterpreter

object LoopInterpreter:
  import Operation.*
  import scala.collection.mutable.Stack

    def eval(p: List[Operation]): Stack[Int] =
      val stack = Stack.empty[Int]
      for op <- p do
        op match
          case Push(n) => stack.push(n)
          case Add() if stack.size >= 2 =>
            val n1 = stack.pop()
            val n2 = stack.pop()
            stack.push(n2 + n1)
          case Substr() if stack.size >= 2 =>
            val n1 = stack.pop()
            val n2 = stack.pop()
            stack.push(n2 - n1)
          case _ => stack
      stack
end LoopInterpreter
