package patmat

object EnumContext:
  enum LookupResult:
    case Ok(v: Int)
    case NotFound

  import LookupResult.*
  enum Context:
    case Empty
    case Cons(name: String, value: Int, tail: Context)

  import Context.*

  def empty: Context =
    Empty

  def cons(name: String, value: Int, rem: Context) =
    Cons(name, value, rem)

  def lookup(ctx: Context, name: String): LookupResult =
    ctx match
      case Empty => NotFound
      case Cons(n, v, tail) =>
        if n == name then Ok(v)
        else lookup(tail, name)

  def erase(ctx: Context, name: String): Context = 
    ctx match
      case Empty => Empty
      case Cons(n, v, tail) =>
        if n == name then erase(tail, name)
        else cons(n, v, erase(tail, name))
    
  def filter(ctx: Context, pred: (String, Int) => Boolean): Context =
    ctx match
      case Empty => Empty
      case Cons(n, v, tail) =>
        if pred(n, v) then cons(n, v, filter(tail, pred))
        else filter(tail, pred)


