package patmat

import UnaryNat.*

object UnaryNatOps:

  def add(n: UnaryNat, m: UnaryNat): UnaryNat =
    n match
      case Zero => m
      case Succ(n0) => Succ(add(n0, m))

  def multiply(n: UnaryNat, m: UnaryNat): UnaryNat =
    n match
      case Zero => Zero
      case Succ(n0) => add(m, multiply(n0, m))

  def minus(n: UnaryNat, m: UnaryNat): UnaryNat =
    n match
      case Zero => Zero
      case Succ(n0) =>
        m match
          case Zero => n
          case Succ(m0) => minus(n0, m0)

  def isEven(n: UnaryNat): Boolean =
    n match
      case Zero => true
      case Succ(n0) => isOdd(n0)
    

  def isOdd(n: UnaryNat): Boolean =
    n match
      case Zero => false
      case Succ(n0) => isEven(n0)

  def fromInt(n: Int): UnaryNat =
    n match
      case 0 => Zero
      case n => Succ(fromInt(n - 1))
    