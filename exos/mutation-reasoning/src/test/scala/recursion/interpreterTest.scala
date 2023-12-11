package recursion.interpreter

class RecursiveInterpreterTest extends munit.FunSuite:
  import RecursiveInterpreter.*
  import Operation.*

  test("evalOp: Push") {
    evalOp(List(2, 3), Push(1)) == List(1, 2, 3)
  }

  test("evalOp: Add") {
    evalOp(List(2, 3), Add()) == List(5)
  }

  test("evalOp: Substr") {
    evalOp(List(2, 3), Substr()) == List(-1)
  }

  test("eval: empty program") {
    eval(Nil) == Nil
  }

  test("eval: program with one Push") {
    eval(List(Push(1))) == List(1)
  }

  test("eval: program with one Add") {
    eval(List(Add())) == Nil
  }

  test("eval: program with one Substr") {
    eval(List(Substr())) == Nil
  }

  test("eval: program 2 + 1") {
    eval(List(Push(2), Push(1), Add())) == List(3)
  }

  test("eval: program 2 - 1") {
    eval(List(Push(2), Push(1), Substr())) == List(1)
  }

  test("eval: program (3 + 5) - 6") {
    eval(List(Push(3), Push(5), Add(), Push(6), Substr())) == List(2)
  }

end RecursiveInterpreterTest

class LoopInterpreterTest extends munit.FunSuite:
  import LoopInterpreter.*
  import Operation.*

  test("eval: empty program") {
    eval(Nil) == Nil
  }

  test("eval: program with one Push") {
    eval(List(Push(1))) == List(1)
  }

  test("eval: program with one Add") {
    eval(List(Add())) == Nil
  }

  test("eval: program with one Substr") {
    eval(List(Substr())) == Nil
  }

  test("eval: program 2 + 1") {
    eval(List(Push(2), Push(1), Add())) == List(3)
  }

  test("eval: program 2 - 1") {
    eval(List(Push(2), Push(1), Substr())) == List(1)
  }

  test("eval: program (3 + 5) - 6") {
    eval(List(Push(3), Push(5), Add(), Push(6), Substr())) == List(2)
  }

end LoopInterpreterTest
