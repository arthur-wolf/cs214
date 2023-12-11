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
    ???
end LoopInterpreter
