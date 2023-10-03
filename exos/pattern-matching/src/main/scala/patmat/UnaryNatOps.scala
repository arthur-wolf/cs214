package patmat

import UnaryNat.*

object UnaryNatOps:

  def add(n: UnaryNat, m: UnaryNat): UnaryNat =
    Zero

  def multiply(n: UnaryNat, m: UnaryNat): UnaryNat =
    m match
      case Zero => n
      case Succ(m0) => add(n, multiply(n, m0))

  def minus(n: UnaryNat, m: UnaryNat): UnaryNat =
    n

  def isEven(n: UnaryNat): Boolean =
    false

  def isOdd(n: UnaryNat): Boolean =
    false



