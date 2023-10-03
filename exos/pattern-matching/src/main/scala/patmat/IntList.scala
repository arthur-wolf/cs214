package patmat

enum IntList:
  case IntNil
  case IntCons(x: Int, xs: IntList)

  def isEmpty: Boolean = this match
    case IntNil => true
    case IntCons(head, tail) => false

  def head: Int = this match
    case IntCons(x, _) => x
    case _ => throw RuntimeException("head of empty")

  def tail: IntList = this match
    case IntCons(_, xs) => xs
    case _ => throw RuntimeException("tail of empty")


