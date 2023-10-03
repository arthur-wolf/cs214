import patmat.*

import IntList.*
import IntListOps.*

class IntListOpsTest extends munit.FunSuite:
  test("polishEval: addition"):
    val xs = IntCons(Add, IntCons(1, IntCons(2, IntNil)))
    assertEquals(polishEval(xs)._1, 3)

  test("polishEval: nested operations"):
    val xs = IntCons(Add, IntCons(2, IntCons(Multiply, IntCons(3, IntCons(4, IntNil)))))
    assertEquals(polishEval(xs)._1, 14)

  test("zipWith: both empty"):
    val res = zipWith(IntNil, IntNil, _ + _)
    assertEquals(res, IntNil)

  test("zipWith: left empty"):
    val res = zipWith(IntNil, IntCons(1, IntNil), _ + _)
    assertEquals(res, IntNil)

  test("zipWith: right empty"):
    val res = zipWith(IntCons(1, IntNil), IntNil, _ + _)
    assertEquals(res, IntNil)

  test("zipWith: same length"):
    val x = IntCons(1, IntCons(2, IntNil))
    val y = IntCons(2, IntCons(3, IntNil))
    val res = zipWith(x, y, _ + _)
    assertEquals(res, IntCons(3, IntCons(5, IntNil)))

  test("zipWith: left longer"):
    val x = IntCons(1, IntCons(2, IntCons(10, IntNil)))
    val y = IntCons(2, IntCons(3, IntNil))
    val res = zipWith(x, y, _ + _)
    assertEquals(res, IntCons(3, IntCons(5, IntNil)))

  test("zipWith: right longer"):
    val x = IntCons(1, IntCons(2, IntNil))
    val y = IntCons(2, IntCons(3, IntCons(10, IntNil)))
    val res = zipWith(x, y, _ + _)
    assertEquals(res, IntCons(3, IntCons(5, IntNil)))

  test("extractSecond: empty"):
    assertEquals(extractSecond(IntNil), ExtractResult.EmptyList)

  test("extractSecond: not long enough"):
    assertEquals(extractSecond(IntCons(1, IntNil)), ExtractResult.NotLongEnough)

  test("extractSecond: good"):
    assertEquals(extractSecond(IntCons(1, IntCons(2, IntNil))), ExtractResult.SecondElem(2))


