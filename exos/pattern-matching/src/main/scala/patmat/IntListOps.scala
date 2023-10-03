package patmat

object IntListOps:
  import IntList.*

  val Add: Int = -1
  val Multiply: Int = -2

  class InvalidOperatorNumber(value: Int) extends RuntimeException
  class InvalidExpression extends RuntimeException

  def polishEval(l: IntList): (Int, IntList) =
    (42, IntNil)

  def zipWith(l1: IntList, l2: IntList, op: (Int, Int) => Int): IntList =
    l1

  enum ExtractResult:
    case SecondElem(i: Int)
    case NotLongEnough
    case EmptyList
  import ExtractResult.*

  def extractSecond(l: IntList): ExtractResult =
    SecondElem(42)


