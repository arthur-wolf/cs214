
package recursion.src.main.scala



final class playground$u002Eworksheet$_ {
def args = playground$u002Eworksheet_sc.args$
def scriptPath = """recursion/src/main/scala/playground.worksheet.sc"""
/*<script>*/
import recursion.*

2 + 2

val oneTwoThree = IntCons(1, IntCons(2, IntCons(3, IntNil())))

oneTwoThree.isEmpty

oneTwoThree.head

oneTwoThree.tail

oneTwoThree.tail.head

// The call below currently fails with a `NotImplementedError`. Go to
// `src/main/scala/recursion/listOps.scala`, implement the `length` function,
// and uncomment the call below to test your implementation.

// length(oneTwoThree)

/*</script>*/ /*<generated>*/
/*</generated>*/
}

object playground$u002Eworksheet_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new playground$u002Eworksheet$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export playground$u002Eworksheet_sc.script as playground$u002Eworksheet

