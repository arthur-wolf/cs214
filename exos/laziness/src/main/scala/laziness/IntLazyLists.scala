package laziness

import MyLazyList.*

object IntLazyLists:
  def from(x: Int): MyLazyList[Int] =
    cons(x, from(x + 1))

  def range(x: Int, y: Int): MyLazyList[Int] =
    from(x).take(y - x)

  val anonymList: MyLazyList[Int] = cons(1, anonymList)
  lazy val infiniteTwoes: MyLazyList[Int] = anonymList.map(_ + 1)
  val naturalNumbers1: MyLazyList[Int] = from(0)
  lazy val naturalNumbers2: MyLazyList[Int] = cons(0, naturalNumbers2.map(_ + 1))
  lazy val primeNumbers: MyLazyList[Int] =
    import MyLazyListState.*
    def sieve(s: MyLazyList[Int]): MyLazyList[Int] = 
      cons(s.head, sieve(s.tail.filter(_ % s.head != 0)))
    sieve(from(2))
  lazy val fib: MyLazyList[Int] = cons(0, cons(1, fib.zip(fib.tail).map((x, y) => x + y)))
