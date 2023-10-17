package poly

import poly.MyList.*
import poly.Instruction.*
import poly.Interpreter.*
import poly.NotEnoughOperandsInStackException

class InterpreterTests extends munit.FunSuite:

  val stack3: MyList[Int] = Cons(3, Cons(5, Cons(-2, Nil)))
  val stack5: MyList[Int] = Cons(4, Cons(-9, Cons(8, Cons(2, Cons(3, Nil)))))
  val prog: Program = Cons(
    Push(2),
    Cons(Push(5), Cons(Push(-3), Cons(Sub, Cons(Mul, Cons(Push(4), Cons(Push(9), Cons(Add, Cons(Add, Nil))))))))
  )

  test("interpreteInst: push to stack"):
    assertEquals(interpreteInst(stack3, Push(7)), Cons(7, stack3))

  test("interpreteInst: pop from empty stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpreteInst(Nil, Pop)

  test("interpreteInst: pop from non-empty stack"):
    assertEquals(interpreteInst(stack3, Pop), Cons(5, Cons(-2, Nil)))

  test("interpreteInst: add in empty stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpreteInst(Nil, Add)

  test("interpreteInst: add in 1-element stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpreteInst(Cons(1, Nil), Add)

  val add: (Int, Int) => Int = (x, y) => x + y
  val sub: (Int, Int) => Int = (x, y) => x - y
  val mul: (Int, Int) => Int = (x, y) => x * y
  val div: (Int, Int) => Int = (x, y) => x / y

  val operations = scala.List(
    ("add", Add, add),
    ("sub", Sub, sub),
    ("mul", Mul, mul),
    ("div", Div, div)
  )

  for (opName, opInst, opFunc) <- operations do
    test(s"interpreteInst: $opName in empty stack"):
      intercept[NotEnoughOperandsInStackException]:
        interpreteInst(Nil, opInst)

    test(s"interpreteInst: $opName in 1-element stack"):
      intercept[NotEnoughOperandsInStackException]:
        interpreteInst(Cons(1, Nil), opInst)

    test(s"interpreteInst: $opName in 5-element stack"):
      assertEquals(Cons(opFunc(stack5.tail.head, stack5.head), stack5.tail.tail), interpreteInst(stack5, opInst))

  test(s"interpreteInst: divide by zero exception"):
    intercept[DivideByZeroException]:
      interpreteInst(Cons(0, Cons(9, Nil)), Div)

  test("interpreteProg: program with initial empty stack"):
    assertEquals(interpreteProg(Nil, prog), Cons(29, Nil))

  test("interpreteProg: program with stack5"):
    assertEquals(interpreteProg(stack5, Cons(Add, Cons(Pop, prog))), Cons(29, stack5.tail.tail))
