package calculator

import scala.util.{Try, Success, Failure}

object FullEvaluator:
  /** Result of evaluation. */
  enum FullEvalResult:
    case Ok(v: Double)
    case DivByZero
    case UndefinedVar(name: String)

    def get: Double = this match
      case Ok(v)              => v
      case DivByZero          => throw new RuntimeException("division by zero")
      case UndefinedVar(name) => throw new RuntimeException(s"undefined variable: $name")

  // Define your own context here
  enum Result:
    case Empty
    case Cons(name: String, value: Double, tail: Result)

    def lookup(name: String): FullEvalResult =
      this match
        case Empty => FullEvalResult.UndefinedVar(name)
        case Cons(n, v, t) => if n == name then FullEvalResult.Ok(v) else t.lookup(name)

  type Ctx =
    Result
    
  def empty: Ctx =
    Result.Empty

  def cons(name: String, value: Double, tail: Ctx) =
    Result.Cons(name, value, tail)

  def fromList(xs: List[(String, Double)]): Ctx =
    xs match
      case Nil           => empty
      case (n, v) :: rem => cons(n, v, fromList(rem))

class FullEvaluator(ctx: FullEvaluator.Ctx) extends Evaluator[FullExpr, FullEvaluator.FullEvalResult]:
  import FullEvaluator.*
  import FullExpr.*
  import FullEvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: FullExpr): FullEvalResult =
    e match
      case Number(value) => Ok(value)
      case Add(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 + v2)
        case (UndefinedVar(name), _) => UndefinedVar(name)
        case (_, UndefinedVar(name)) => UndefinedVar(name)
        case  _ => DivByZero
      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 - v2)
        case (UndefinedVar(name), _) => UndefinedVar(name)
        case (_, UndefinedVar(name)) => UndefinedVar(name)
        case _ => DivByZero
      case Mul(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 * v2)
        case (UndefinedVar(name), _) => UndefinedVar(name)
        case (_, UndefinedVar(name)) => UndefinedVar(name)
        case _ => DivByZero
      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => if v2 == 0 then DivByZero else Ok(v1 / v2)
        case (UndefinedVar(name), _) => UndefinedVar(name)
        case (_, UndefinedVar(name)) => UndefinedVar(name)
        case _ => DivByZero
      case Neg(e) => evaluate(e) match
        case Ok(v) => Ok(-v)
        case (UndefinedVar(name)) => UndefinedVar(name)
        case _ => DivByZero
      case Var(name) => ctx.lookup(name) match
        case Ok(v) => Ok(v)
        case DivByZero => DivByZero
        case UndefinedVar(name) => UndefinedVar(name)