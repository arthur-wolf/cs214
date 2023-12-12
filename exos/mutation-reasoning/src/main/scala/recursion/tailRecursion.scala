package recursion.tailRecursion
import scala.annotation.tailrec

@tailrec
def reverseAppend(l1: List[Int], l2: List[Int]): List[Int] =
  if l1.isEmpty then l2
  else reverseAppend(l1.tail, l1.head :: l2)

def reverseAppendLoop(l1: List[Int], l2: List[Int]): List[Int] =
    var curr = l1
    var rev = l2
    while true do
      if curr.isEmpty then return rev
      rev = curr.head :: rev
      curr = curr.tail
    throw new AssertionError("Unreachable")
      
@tailrec
def sum(l: List[Int], acc: Int = 0): Int =
  if l.isEmpty then acc
  else sum(l.tail, acc + 1)

def sumLoop(l: List[Int]): Int =
  var acc = 0
  var current = l
  while true do
    if current.isEmpty then return acc
    acc += current.head
    current = current.tail
  throw new AssertionError("Unreachable")

@tailrec
def foldLeft(l: List[Int], acc: Int)(f: (Int, Int) => Int): Int =
  if l.isEmpty then acc
  else foldLeft(l.tail, f(acc, l.head))(f)

def foldLeftLoop(l: List[Int], startValue: Int)(f: (Int, Int) => Int): Int =
  var current = l
  var accumulator = startValue
  while true do
    if current.isEmpty then return accumulator
    accumulator = f(accumulator, current.head)
    current = current.tail
  throw new AssertionError("Unreachable")


extension [T](l: List[T])
  def foldt(z: T)(op: (T, T) => T): T =
    var list = l

    while true do
      list match
        // if list.size > 1
        case _ :: _ :: tail =>
          list = list.pairs(op)
        // if list.size == 1
        case a :: Nil => return a
        // if list.size == 0
        case Nil => return z

    throw new AssertionError("Unreachable")

extension [T](l: List[T])
  def pairs(op: (T, T) => T): List[T] =
    // Optional exercise: write it with a `while` loop!

    l match
      case a :: b :: tl => op(a, b) :: tl.pairs(op)
      case _            => l

def map(l: List[Int], f: Int => Int): List[Int] =
  if l.isEmpty then Nil
  else f(l.head) :: map(l.tail, f)

object MapContext:
  enum MutableList:
    case Nil
    case Cons(val hd: Int, var tail: MutableList)

  import MutableList.*

  def mapTR(l: MutableList, f: Int => Int): MutableList =
    l match
      case Nil => Nil
      case Cons(hd, tl) =>
        val acc: Cons = Cons(f(hd), Nil)
        mapTRWorker(tl, f, acc)
        acc

  // @tailrec uncomment when working on the exercise
  def mapTRWorker(
      l: MutableList,
      f: Int => Int,
      acc: MutableList.Cons
  ): Unit =
    l match
      case Nil => ()
      case Cons(h, t) =>
        acc.tail = Cons(f(h), Nil)
        mapTRWorker(t, f, acc.tail.asInstanceOf[Cons])