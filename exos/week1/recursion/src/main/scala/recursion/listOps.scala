package recursion

def length(l: IntList): Int = 
  if (l == IntNil()) 0 
  else 1 + length(l.tail)

def sum(l: IntList): Int =
  if (l == IntNil()) 0 
  else l.head + sum(l.tail)

def product(l: IntList): Int =
  if (l == IntNil()) 1 
  else l.head * product(l.tail)

def horner(l: IntList, x: Int): Int =
  if (l == IntNil()) 0 
  else l.head + x * horner(l.tail, x)

def allEven(l: IntList): Boolean =
  if (l == IntNil()) true
  else (l.head % 2 == 0) && allEven(l.tail)

def allPositiveOrZero(l: IntList): Boolean =
  if (l == IntNil()) true
  else (l.head >= 0) && allPositiveOrZero(l.tail)

def anyOdd(l: IntList): Boolean =
  if (l == IntNil()) false
  else (l.head % 2 == 1) || anyOdd(l.tail)

def anyNegative(l: IntList): Boolean =
  if (l == IntNil()) false
  else (l.head < 0) || anyNegative(l.tail)

def countEven(l: IntList): Int =
  if (l == IntNil()) 0
  else if (l.head % 2 == 0) 1 + countEven(l.tail)
  else countEven(l.tail)

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  length(collectEven(l))

def countPositive(l: IntList): Int =
  if (l == IntNil()) 0
  else if (l.head >= 0) 1 + countPositive(l.tail)
  else countPositive(l.tail)

def increment(l: IntList): IntList = 
  if (l == IntNil()) IntNil() 
  else IntCons(l.head + 1, increment(l.tail))

def decrement(l: IntList): IntList =
 if (l == IntNil()) IntNil()
 else IntCons(l.head - 1, decrement(l.tail))

def collectEven(l: IntList): IntList = 
  if (l == IntNil()) IntNil() 
  else if (l.head % 2 == 0) IntCons(l.head, collectEven(l.tail)) 
  else collectEven(l.tail)

def removeOdd(l: IntList): IntList =
  if (l == IntNil()) IntNil()
  else if (l.head % 2 == 0) IntCons(l.head, removeOdd(l.tail))
  else removeOdd(l.tail)

def reverseAppend(l1: IntList, l2: IntList): IntList =
  if (l1 == IntNil()) l2
  else reverseAppend(l1.tail, IntCons(l1.head, l2))

def reverse(l: IntList): IntList =
  reverseAppend(l, IntNil())

def last(l: IntList): Int =
  if (l == IntNil()) throw IllegalArgumentException("empty list")
  else if (l.tail == IntNil()) l.head
  else last(l.tail)

def minMax(l: IntList): (Int, Int) =
  if (l == IntNil()) throw IllegalArgumentException("empty list")
  else if (l.tail == IntNil()) (l.head, l.head)
  else {
    val (min, max) = minMax(l.tail)
    (if (l.head < min) l.head else min, if (l.head > max) l.head else max)
  }


val Add = -1
val Multiply = -2

def polishEval(l: IntList): (Int, IntList) =
  ???