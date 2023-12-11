package rec

object NatRec:
  def natRec[T](baseCase: => T)(recCase: T => T)(n: Int): T =
    if n == 0 then baseCase
    else recCase(natRec(baseCase)(recCase)(n - 1))

  def wow(n: Int): String =
    if n == 0 then "" // base case
    else "!" + wow(n - 1) // recursive case

  val wowComb: Int => String = natRec("")(acc => "!" + acc)

  def even(n: Int): Boolean =
    if n == 0 then true // base case
    else !even(n - 1) // recursive case

  val evenComb: Int => Boolean =
    TODO

object ListRec:
  def listRec[T, U](nilCase: => U)(consCase: (T, U) => U)(l: List[T]): U =
    ???

  val increment =
    listRec[Int, List[Int]](Nil)((h, acc) => h + 1 :: acc)

  val allPositive =
    listRec[Int, Boolean](true)((h, acc) => h > 0 && acc)

object TreeRec:
  enum Tree[T]:
    case Leaf(t: T)
    case Branch(l: Tree[T], r: Tree[T])
  import Tree.*

  def treeRec[T, U](leafCase: T => U)(branchCase: (U, U) => U)(tr: Tree[T]): U =
    ???

  val treeMax =
    treeRec[Int, Int](n => n)((maxl, maxr) => math.max(maxl, maxr))

  val treeToString =
    treeRec[Int, String](_.toString)((strl, strr) => strl ++ strr)

object FixpointCombinator:
  def fixpoint[A, B](f: (A, A => B) => B)(a: A): B =
    ???

object Memo:
  import scala.collection.mutable

  def memo[A, B](f: (A, A => B) => B)(a: A): B =
    ???

  val fib = memo: (n: Int, f: Int => Int) =>
    if n <= 1 then 1 else f(n - 1) + f(n - 2)

  val choose = memo[(Int, Int), Int] {
    case ((n, k), f) =>
      if k <= 0 || k >= n then 1
      else f((n - 1, k - 1)) + f((n - 1, k))
  }
