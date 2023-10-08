package calculator

object Simplifier:
  import FullExpr.*

  /** Fold constant sub-expressions in values. */
  def constfold(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (constfold(e1), constfold(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 + v2)
        case (e1, e2) => Add(e1, e2)
      case Minus(e1, e2) => (constfold(e1), constfold(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 - v2)
        case (e1, e2) => Minus(e1, e2)
      case Mul(e1, e2) => (constfold(e1), constfold(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 * v2)
        case (e1, e2) => Mul(e1, e2)
      case Div(e1, e2) => (constfold(e1), constfold(e2)) match
        case (Number(v1), Number(v2)) => Number(v1 / v2)
        case (e1, e2) => Div(e1, e2)
      case Neg(e) => Neg(constfold(e))
      case Var(name) => Var(name)
    

  // simplification rules
  // 1. 0 + e = e + 0 = e
  // 2. 0 - e = -e
  // 3. e - 0 = e
  // 4. 0 * e = e * 0 = 0
  // 5. 1 * e = e * 1 = e
  // 6. e / 1 = e 
  // 7. e - e = 0
  /** Simplifiy expressions based on the listed algebraic rules. */
  def algebraic(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (algebraic(e1), algebraic(e2)) match
        case (Number(0), e2) => e2
        case (e1, Number(0)) => e1
        case (e1, e2) => Add(e1, e2)
      case Minus(e1, e2) => (algebraic(e1), algebraic(e2)) match
        case (Number(0), e2) => Neg(e2)
        case (e1, Number(0)) => e1
        case (e1, e2) => if e1 == e2 then Number(0) else Minus(e1, e2)
      case Mul(e1, e2) => (algebraic(e1), algebraic(e2)) match
        case (Number(0), _) => Number(0)
        case (_, Number(0)) => Number(0)
        case (Number(1), e2) => e2
        case (e1, Number(1)) => e1
        case (e1, e2) => Mul(e1, e2)
      case Div(e1, e2) => (algebraic(e1), algebraic(e2)) match
        case (e1, Number(1)) => e1
        case (e1, e2) => Div(e1, e2)
      case Neg(e) => Neg(algebraic(e))
      case Var(name) => Var(name)
    

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(value) => Number(value)
      case Add(e1, e2) => (simplify(e1), simplify(e2)) match
        case (e1, Neg(Number(0))) => e1
        case (e1, Number(0)) => e1
        case (Neg(Number(0)), e2) => e2
        case (Number(0), e2) => e2
        case (Number(v1), Number(v2)) => Number(v1 + v2)
        case (e1, e2) => Add(e1, e2)
      case Minus(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(0), Number(0)) => Number(0)
        case (Number(0), e2) => Neg(e2)
        case (e1, Number(0)) => e1
        case (Number(v1), Number(v2)) => Number(v1 - v2)
        case (e1, e2) => if e1 == e2 then Number(0) else Minus(e1, e2)
      case Mul(e1, e2) => (simplify(e1), simplify(e2)) match
        case (Number(0), _) => Number(0)
        case (_, Number(0)) => Number(0)
        case (Number(1), e2) => e2
        case (e1, Number(1)) => e1
        case (Number(v1), Number(v2)) => Number(v1 * v2)
        case (e1, e2) => Mul(e1, e2)
      case Div(e1, e2) => (simplify(e1), simplify(e2)) match
        case (e1, Number(1)) => e1
        case (Number(v1), Number(v2)) => Number(v1 / v2)
        case (e1, e2) => Div(e1, e2)
      case Neg(e) => simplify(e) match
        case Neg(Number(0)) => Number(0)
        case e => Neg(e)
      case Var(name) => Var(name)
    