package poly

final class EmptyListException extends Exception(f"Empty list")

enum MyList[+A]:
  case Nil
  case Cons(x: A, xs: MyList[A])

  def isEmpty: Boolean = this match
    case Nil => true
    case _   => false

  def head: A = this match
    case Nil        => throw EmptyListException()
    case Cons(x, _) => x

  def tail: MyList[A] = this match
    case Nil         => throw EmptyListException()
    case Cons(_, xs) => xs

object MyList:
  def from[A](it: Iterable[A]): MyList[A] =
    it.foldRight(Nil: MyList[A])(Cons(_, _))

enum IntList:
  case IntNil
  case IntCons(x: Int, xs: IntList)

object ListSigs:
  import IntList.*

  def map(f: Int => Int)(l: IntList): IntList =
    l match
      case IntNil         => IntNil
      case IntCons(x, xs) => IntCons(f(x), map(f)(xs))
  import MyList.*

  def map[A, B](f: A => B)(l: MyList[A]): MyList[B] =
    l match
      case Nil         => Nil
      case Cons(x, xs) => Cons(f(x), map(f)(xs))
