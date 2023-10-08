package calculator

class BasicEvaluator extends Evaluator[BasicExpr, BasicEvaluator.EvalResult]:
  import BasicExpr.*
  import BasicEvaluator.*
  import EvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: BasicExpr): EvalResult =
    e match
      case Number(value) => Ok(value)
      case Add(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 + v2)
        case _ => DivByZero
      case Minus(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 - v2)
        case _ => DivByZero
      case Mul(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => Ok(v1 * v2)
        case _ => DivByZero
      case Div(e1, e2) => (evaluate(e1), evaluate(e2)) match
        case (Ok(v1), Ok(v2)) => if v2 == 0 then DivByZero else Ok(v1 / v2)
        case _ => DivByZero
      case Neg(e) => evaluate(e) match
        case Ok(v) => Ok(-v)
        case _ => DivByZero
    
object BasicEvaluator:
  enum EvalResult:
    case Ok(v: Double)
    case DivByZero

    def get: Double = this match
      case Ok(v)     => v
      case DivByZero => throw new RuntimeException(s"division by zero")
