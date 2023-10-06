package patmat

object IntListOps:
  import IntList.*

  val Add: Int = -1
  val Multiply: Int = -2

  class InvalidOperatorNumber(value: Int) extends RuntimeException
  class InvalidExpression extends RuntimeException

  def polishEval(l: IntList): (Int, IntList) =
    l match
      case IntNil => throw InvalidExpression()
      case IntCons(Add, tail) =>
        val (leftVal, leftRem) = polishEval(tail)
        val (rightVal, rightRem) = polishEval(leftRem)
        (leftVal + rightVal, rightRem)
      case IntCons(Multiply, tail) =>
        val (leftVal, leftRem) = polishEval(tail)
        val (rightVal, rightRem) = polishEval(leftRem)
        (leftVal * rightVal, rightRem)
      case IntCons(x, tail) =>
        if x >= 0 then (x, tail) else throw InvalidOperatorNumber(x)
    
    

  def zipWith(l1: IntList, l2: IntList, op: (Int, Int) => Int): IntList =
    (l1, l2) match
      case (IntNil, IntNil) => IntNil
      case (IntCons(_, _), IntNil) => IntNil
      case (IntNil, IntCons(_, _)) => IntNil
      case (IntCons(x, xs), IntCons(y, ys)) =>
        IntCons(op(x, y), zipWith(xs, ys, op))

  enum ExtractResult:
    case SecondElem(i: Int)
    case NotLongEnough
    case EmptyList
  import ExtractResult.*

  def extractSecond(l: IntList): ExtractResult =
    l match
      case IntNil => EmptyList
      case IntCons(_, IntNil) => NotLongEnough
      case IntCons(_, IntCons(x, _)) => SecondElem(x)


