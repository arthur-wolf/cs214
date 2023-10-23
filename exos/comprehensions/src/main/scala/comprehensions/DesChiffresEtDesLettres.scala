package comprehensions

object DesLettres:
  // start scramble
  def scramble(word: String): String =
    word.toUpperCase().sorted
  // end scramble

  // start scrambleList
  def scrambleList(allWords: Set[String]): Map[String, Set[String]] =
    allWords.groupBy(scramble)
  // end scrambleList

  // start exactWord
  def exactWord(allWords: Set[String], letters: String): Set[String] =
    scrambleList(allWords).getOrElse(scramble(letters), Set.empty)
  // end exactWord

  // start compatible
  def compatible(small: String, large: String): Boolean =
    ???
  // end compatible

  // start longestWord
  def longestWord(allWords: Set[String], letters: String): Set[String] =
    ???
  // end longestWord

object DesChiffres:
  // start expr
  trait Expr:
    val value: Option[Int]
  // end expr

  // start num
  case class Num(n: Int) extends Expr:
    val value = Some(n)
    // end num
    override def toString(): String = f"$n" // Print as number

  // start binop
  abstract class Binop extends Expr:
    val e1, e2: Expr // Subexpressions
    def op(n1: Int, n2: Int): Option[Int] // How to evaluate this operator
    // end binop

    val opStr: String // How to print this operator
    override def toString(): String = f"($e1 $opStr $e2)"

    // start binop-value
    val value: Option[Int] =
      for
        n1 <- e1.value
        n2 <- e2.value
        r <- op(n1, n2)
      yield r
    // end binop-value

  // start expr-add
  case class Add(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      Some(n1 + n2)
    // end expr-add
    val opStr = "+"

  // start expr-sub
  case class Sub(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      if n1 < n2 then None else Some(n1 - n2)
    // end expr-sub
    val opStr = "-"

  // start expr-mul
  case class Mul(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      Some(n1 * n2)
    // end expr-mul
    val opStr = "*"

  // start expr-div
  case class Div(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      if n2 != 0 && n1 % n2 == 0 then Some(n1 / n2) else None
    // end expr-div
    val opStr = "/"

  // start partitions
  def partitions[A](l: List[A]): List[(List[A], List[A])] =
      ???
  // end partitions

  // start allTrees
  def allTrees(ints: List[Int]): List[Expr] =
      ???
  // end allTrees

  // start leCompteEstBon
  def leCompteEstBon(ints: List[Int], target: Int): Option[Expr] =
      ???
  // end leCompteEstBon
