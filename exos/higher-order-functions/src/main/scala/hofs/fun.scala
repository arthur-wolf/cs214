package hofs

def headIsEven(l: IntList) =
  !l.isEmpty && l.head % 2 == 0

def headIsPositive(l: IntList) =
  !l.isEmpty && l.head > 0


def headHasProperty(p: Int => Boolean, l: IntList) =
  !l.isEmpty && p(l.head)

def headHasPropertyCurried(p: Int => Boolean) =
  (l: IntList) => !l.isEmpty && p(l.head)


def headIsEven1(l: IntList) =
  headHasProperty(i => i % 2 == 0, l)

def headIsPositive1(l: IntList) =
  headHasProperty(i => i > 0, l)

val headIsEven2 =
  headHasPropertyCurried(i => i % 2 == 0)
val headIsPositive2 =
  headHasPropertyCurried(i => i > 0)

def DoubleTriple(x: Int) =
  IntCons(x * 2, IntCons(x * 3, IntNil()))

def DivideTrivide(x: Int) =
  IntCons(x / 2, IntCons(x / 3, IntNil()))

def IncrementDeuxcrement(x: Int) =
  IntCons(x + 1, IntCons(x + 2, IntNil()))

def ConstructTwo(f: Int => Int, g: Int => Int): Int => IntList =
  x => IntCons(f(x), IntCons(g(x), IntNil()))

val DoubleTriple2 = ConstructTwo(x => x * 2, x => x * 3)
val DivideTrivide2 = ConstructTwo(x => x / 2, x => x / 3)
val IncrementDeuxcrement2 = ConstructTwo(x => x + 1, x => x + 2)

def andThen(f: Int => Double, g: Double => String) =
  (x: Int) => g(f(x))

val id: Int => Int =
  x => x

def flip(f: (Int, Int) => Int): (Int, Int) => Int =
  (x, y) => f(y, x)

val square = (x: Int) => x * x
val plusOne = (x: Int) => x + 1
val minusOne = (x: Int) => x - 1
def composeInt(f: Int => Int, g: Int => Int): Int => Int =
  x => f(g(x))

val squareMinusOne     = (x: Int) => (x - 1) * (x - 1)
val squarePlusOne      = (x: Int) => (x + 1) * (x + 1)
val squareSquare       = (x: Int) => (x * x) * (x * x)
val squareMinusTwo     = (x: Int) => (x - 2) * (x - 2)
val squareSquareSquare = (x: Int) =>
  ((x * x) * (x * x)) * ((x * x) * (x * x))

def adder(f: Int => Double, g: Int => Double): Int => Double =
  x => f(x) + g(x)

def multiplier(f: Int => Double, g: Int => Double): Int => Double =
  x => f(x) * g(x)

def lifter(op: (Double, Double) => Double)
    : (Int => Double, Int => Double) => (Int => Double) =
  (f, g) => x => op(f(x), g(x))

val adder2 = lifter((x, y) => x + y)
val multiplier2 = lifter((x, y) => x * y)

def meet(f: Int => Boolean, g: Int => Boolean)
    : (Int => Boolean) =
  x => f(x) && g(x)

def Meet(l: IntPredicateList): (Int => Boolean) =
  if l.isEmpty then
    x => true
  else
    meet(l.head, Meet(l.tail))

val f0 = (x: Long) => x
val f1 = (x: Long) => if x > 0 then x else -x
val f2 = (x: Long) => x + 1 - 1
val f3 = (x: Long) =>
  Math.sqrt(x.toDouble * x.toDouble).round
val f4: Long => Long = x =>
  if x < 0 then f4(x + 1) - 1
  else if x > 0 then f4(x - 1) + 1
  else 0

def eqBoolBool(
    f: Boolean => Boolean,
    g: Boolean => Boolean
) =
  ???

val a = (x: Int) => x
val b = (x: Int) => -x
val c = (x: Int) => x + 1
val d = (x: Int) => (x / 2) + 5
val e = (x: Int) => if x % 10 == 0 then x else (x + 1)
val f = (x: Int) => -(x * x)
val g = (x: Int) => /* 🔥 */ /* assuming x > 0 */
  if x == 1 then 1
  else if x % 2 == 0 then x / 2
  else 3 * x + 1

import scala.annotation.tailrec

def fixedPoint(f: Int => Int, start: Int): Int =
  if f(start) == start then start
  else fixedPoint(f, f(start))

def mapAsFoldRight(f: Int => Int): IntList => IntList =
  ???

def filterAsFoldRight(p: Int => Boolean): IntList => IntList =
  ???

def forallNoIf(p: Int => Boolean)(l: IntList): Boolean =
  ???

def existsNoIf(p: Int => Boolean)(l: IntList): Boolean =
  ???
