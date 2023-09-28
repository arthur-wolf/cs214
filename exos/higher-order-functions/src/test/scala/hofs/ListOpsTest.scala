package hofs

class ListOpsTests extends munit.FunSuite:
  val twoNumbers: IntList =
    IntCons(-3, IntCons(-5, IntNil()))

  val fourNumbers: IntList =
    IntCons(1, IntCons(2, IntCons(-3, IntCons(4, IntNil()))))

  test("length: empty list"):
    assertEquals(length(IntNil()), 0)

  test("length: list with 2 elements"):
    assertEquals(length(twoNumbers), 2)

  test("length: list with 4 elements"):
    assertEquals(length(fourNumbers), 4)

  test("allPositiveOrZero: empty list"):
    assertEquals(allPositiveOrZero(IntNil()), true)

  test("allPositiveOrZero: list with 2 elements"):
    assertEquals(allPositiveOrZero(twoNumbers), false)

  test("allPositiveOrZero: list with 4 elements"):
    assertEquals(allPositiveOrZero(fourNumbers), false)

  test("countPositive: empty list"):
    assertEquals(countPositive(IntNil()), 0)

  test("countPositive: list with 2 elements"):
    assertEquals(countPositive(twoNumbers), 0)

  test("countPositive: list with 4 elements"):
    assertEquals(countPositive(fourNumbers), 3)

  test("sum: empty list"):
    assertEquals(sum(IntNil()), 0)

  test("sum: list with 2 elements"):
    assertEquals(sum(twoNumbers), -8)

  test("sum: list with 4 elements"):
    assertEquals(sum(fourNumbers), 4)

  test("product: empty list"):
    assertEquals(product(IntNil()), 1)

  test("product: list with 2 elements"):
    assertEquals(product(twoNumbers), 15)

  test("product: list with 4 elements"):
    assertEquals(product(fourNumbers), -24)

  test("anyOdd: empty list"):
    assertEquals(anyOdd(IntNil()), false)

  test("anyOdd: list with 2 elements"):
    assertEquals(anyOdd(twoNumbers), true)

  test("anyOdd: list with 4 elements"):
    assertEquals(anyOdd(fourNumbers), true)

  test("decrement: empty list"):
    assertListEquals(decrement(IntNil()), IntNil())

  test("decrement: list with 2 elements"):
    assertListEquals(decrement(twoNumbers), IntCons(-4, IntCons(-6, IntNil())))

  test("decrement: list with 4 elements"):
    assertListEquals(
      decrement(fourNumbers),
      IntCons(0, IntCons(1, IntCons(-4, IntCons(3, IntNil()))))
    )

  test("collectEven: empty list"):
    assertListEquals(collectEven(IntNil()), IntNil())

  test("collectEven: list with 2 elements"):
    assertListEquals(collectEven(twoNumbers), IntNil())

  test("collectEven: list with 4 elements"):
    assertListEquals(
      collectEven(fourNumbers),
      IntCons(2, IntCons(4, IntNil()))
    )

  test("min: empty list"):
    intercept[IllegalArgumentException]:
      min(IntNil())

  test("min: list with 2 elements"):
    assertEquals(min(twoNumbers), -5)

  test("min: list with 4 elements"):
    assertEquals(min(fourNumbers), -3)

  test("increment: empty list"):
    assertListEquals(increment(IntNil()), IntNil())

  test("increment: list with 2 elements"):
    assertListEquals(increment(twoNumbers), IntCons(-2, IntCons(-4, IntNil())))

  test("increment: list with 4 elements"):
    assertListEquals(
      increment(fourNumbers),
      IntCons(2, IntCons(3, IntCons(-2, IntCons(5, IntNil()))))
    )

  test("subtract: empty list"):
    intercept[IllegalArgumentException]:
      subtract(IntNil())

  test("subtract: list with 2 elements"):
    assertEquals(subtract(twoNumbers), 2)

  test("subtract: list with 4 elements"):
    assertEquals(subtract(fourNumbers), -8)

  test("removeOdd: empty list"):
    assertListEquals(removeOdd(IntNil()), IntNil())

  test("removeOdd: list with 2 elements"):
    assertListEquals(removeOdd(twoNumbers), IntNil())

  test("removeOdd: list with 4 elements"):
    assertListEquals(
      removeOdd(fourNumbers),
      IntCons(2, IntCons(4, IntNil()))
    )

  test("countEven: empty list"):
    assertEquals(countEven(IntNil()), 0)

  test("countEven: list with 2 elements"):
    assertEquals(countEven(twoNumbers), 0)

  test("countEven: list with 4 elements"):
    assertEquals(countEven(fourNumbers), 2)

  test("countEven2: empty list"):
    assertEquals(countEven2(IntNil()), 0)

  test("countEven2: list with 2 elements"):
    assertEquals(countEven2(twoNumbers), 0)

  test("countEven2: list with 4 elements"):
    assertEquals(countEven2(fourNumbers), 2)

  test("multiplyBy2: empty list"):
    assertListEquals(multiplyBy2(IntNil()), IntNil())

  test("multiplyBy2: list with 2 elements"):
    assertListEquals(
      multiplyBy2(twoNumbers),
      IntCons(-6, IntCons(-10, IntNil()))
    )

  test("multiplyBy2: list with 4 elements"):
    assertListEquals(
      multiplyBy2(fourNumbers),
      IntCons(2, IntCons(4, IntCons(-6, IntCons(8, IntNil()))))
    )

  test("anyNegative: empty list"):
    assertEquals(anyNegative(IntNil()), false)

  test("anyNegative: list with 2 elements"):
    assertEquals(anyNegative(twoNumbers), true)

  test("anyNegative: list with 4 elements"):
    assertEquals(anyNegative(fourNumbers), true)

  test("allEven: empty list"):
    assertEquals(allEven(IntNil()), true)

  test("allEven: list with 2 elements"):
    assertEquals(allEven(twoNumbers), false)

  test("allEven: list with 4 elements"):
    assertEquals(allEven(fourNumbers), false)

  test("multiplyOdd: empty list"):
    assertEquals(multiplyOdd(IntNil()), 1)

  test("multiplyOdd: list with 2 elements"):
    assertEquals(multiplyOdd(twoNumbers), 15)

  test("multiplyOdd: list with 4 elements"):
    assertEquals(multiplyOdd(fourNumbers), -3)

  test("horner: empty list"):
    assertEquals(horner(3, IntNil()), 0)

  test("horner: list with 2 elements"):
    assertEquals(horner(3, twoNumbers), -18)

  test("horner: list with 4 elements"):
    assertEquals(horner(3, fourNumbers), 88)

  test("capAtZero: empty list"):
    assertEquals(capAtZero(IntNil()), IntNil())

  test("capAtZero: list with 2 elements"):
    assertEquals(capAtZero(twoNumbers), IntCons(-3, IntCons(-5, IntNil())))

  test("capAtZero: list with 4 elements"):
    assertEquals(capAtZero(fourNumbers), IntCons(0, IntCons(0, IntCons(-3, IntCons(0, IntNil())))))

  test("removeZeroes: empty list"):
    assertEquals(removeZeroes(IntNil()), IntNil())

  test("removeZeroes: list with 2 elements"):
    assertEquals(removeZeroes(twoNumbers), twoNumbers)

  test("removeZeroes: list with 4 elements"):
    assertEquals(removeZeroes(fourNumbers), fourNumbers)

  test("reverseAppend: empty lists"):
    assertEquals(reverseAppend(IntNil(), IntNil()), IntNil())

  test("reverseAppend: list with 2 elements to empty list"):
    assertEquals(reverseAppend(twoNumbers, IntNil()), IntCons(-5, IntCons(-3, IntNil())))

  test("reverseAppend: list with 4 elements to empty list"):
    assertEquals(
      reverseAppend(fourNumbers, IntNil()),
      IntCons(4, IntCons(-3, IntCons(2, IntCons(1, IntNil()))))
    )

  test("reverseAppend: empty list to 4 elements"):
    assertEquals(reverseAppend(IntNil(), twoNumbers), twoNumbers)

  test("reverseAppend: empty list to 4 elements"):
    assertEquals(reverseAppend(IntNil(), fourNumbers), fourNumbers)

  test("reverseAppend: 4 elements to 2 elements"):
    assertEquals(
      reverseAppend(fourNumbers, twoNumbers),
      IntCons(4, IntCons(-3, IntCons(2, IntCons(1, twoNumbers))))
    )

  test("reverseAppend: 2 elements to 4 elements"):
    assertEquals(
      reverseAppend(twoNumbers, fourNumbers),
      IntCons(-5, IntCons(-3, fourNumbers))
    )

  test("reverse: list with 2 elements"):
    assertEquals(reverse(twoNumbers), IntCons(-5, IntCons(-3, IntNil())))

  test("reverse: list with 4 elements"):
    assertEquals(
      reverse(fourNumbers),
      IntCons(4, IntCons(-3, IntCons(2, IntCons(1, IntNil()))))
    )

  test("takeWhilePositive: empty list"):
    assertEquals(takeWhilePositive(IntNil()), IntNil())

  test("takeWhilePositive: list with 2 elements"):
    assertEquals(takeWhilePositive(twoNumbers), IntNil())

  test("takeWhilePositive: list with 4 elements"):
    assertEquals(takeWhilePositive(fourNumbers), IntCons(1, IntCons(2, IntNil())))

  test("append: empty lists"):
    assertEquals(append(IntNil(), IntNil()), IntNil())

  test("append: list with 2 elements to empty list"):
    assertEquals(append(twoNumbers, IntNil()), twoNumbers)

  test("append: list with 4 elements to empty list"):
    assertEquals(append(fourNumbers, IntNil()), fourNumbers)

  test("append: empty list to 4 elements"):
    assertEquals(append(IntNil(), twoNumbers), twoNumbers)

  test("append: empty list to 4 elements"):
    assertEquals(append(IntNil(), fourNumbers), fourNumbers)

  test("append: 4 elements to 2 elements"):
    assertEquals(
      append(fourNumbers, twoNumbers),
      IntCons(1, IntCons(2, IntCons(-3, IntCons(4, twoNumbers))))
    )

  test("append: 2 elements to 4 elements"):
    assertEquals(
      append(twoNumbers, fourNumbers),
      IntCons(-3, IntCons(-5, fourNumbers))
    )

  test("appendUsingReverseAppend: empty lists"):
    assertEquals(appendUsingReverseAppend(IntNil(), IntNil()), IntNil())

  test("appendUsingReverseAppend: list with 2 elements to empty list"):
    assertEquals(appendUsingReverseAppend(twoNumbers, IntNil()), twoNumbers)

  test("appendUsingReverseAppend: list with 4 elements to empty list"):
    assertEquals(appendUsingReverseAppend(fourNumbers, IntNil()), fourNumbers)

  test("appendUsingReverseAppend: empty list to 4 elements"):
    assertEquals(appendUsingReverseAppend(IntNil(), twoNumbers), twoNumbers)

  test("appendUsingReverseAppend: empty list to 4 elements"):
    assertEquals(appendUsingReverseAppend(IntNil(), fourNumbers), fourNumbers)

  test("appendUsingReverseAppend: 4 elements to 2 elements"):
    assertEquals(
      appendUsingReverseAppend(fourNumbers, twoNumbers),
      IntCons(1, IntCons(2, IntCons(-3, IntCons(4, twoNumbers))))
    )

  test("appendUsingReverseAppend: 2 elements to 4 elements"):
    assertEquals(
      appendUsingReverseAppend(twoNumbers, fourNumbers),
      IntCons(-3, IntCons(-5, fourNumbers))
    )

  test("collectMultiples: empty list"):
    assertEquals(collectMultiples(2, IntNil()), IntNil())

  test("collectMultiples: list with 2 elements"):
    assertEquals(collectMultiples(2, twoNumbers), IntNil())

  test("collectMultiples: list with 4 elements"):
    assertEquals(collectMultiples(2, fourNumbers), IntCons(2, IntCons(4, IntNil())))

  test("last: empty list"):
    intercept[IllegalArgumentException]:
      last(IntNil())

  test("last: list with 2 elements"):
    assertEquals(last(twoNumbers), -5)

  test("last: list with 4 elements"):
    assertEquals(last(fourNumbers), 4)

  test("init: empty list"):
    intercept[IllegalArgumentException]:
      init(IntNil())

  test("init: list with 2 elements"):
    assertEquals(init(twoNumbers), IntCons(-3, IntNil()))

  test("init: list with 4 elements"):
    assertEquals(init(fourNumbers), IntCons(1, IntCons(2, IntCons(-3, IntNil()))))

  test("contains: empty list"):
    assertEquals(contains(IntNil(), 2), false)

  test("contains: list with fourElements, false"):
    assertEquals(contains(fourNumbers, -1), false)

  test("contains: list with fourElements, first"):
    assertEquals(contains(fourNumbers, 1), true)

  test("contains: list with fourElements, second"):
    assertEquals(contains(fourNumbers, 2), true)

  test("contains: list with fourElements, third"):
    assertEquals(contains(fourNumbers, -3), true)

  test("contains: list with fourElements, fourth"):
    assertEquals(contains(fourNumbers, 4), true)

  test("isSubset: empty list"):
    assertEquals(isSubset(IntNil(), fourNumbers), true)

  test("isSubset: first"):
    assertEquals(isSubset(IntCons(1, IntNil()), fourNumbers), true)

  test("isSubset: last"):
    assertEquals(isSubset(IntCons(4, IntNil()), fourNumbers), true)

  test("isSubset: false"):
    assertEquals(isSubset(twoNumbers, fourNumbers), false)

  test("intersection: empty list"):
    assertEquals(intersection(IntNil(), fourNumbers), IntNil())
    assertEquals(intersection(fourNumbers, IntNil()), IntNil())

  test("intersection: first"):
    assertEquals(intersection(IntCons(1, IntNil()), fourNumbers), IntCons(1, IntNil()))
    assertEquals(intersection(fourNumbers, IntCons(1, IntNil())), IntCons(1, IntNil()))

  test("intersection: last"):
    assertEquals(intersection(IntCons(4, IntNil()), fourNumbers), IntCons(4, IntNil()))
    assertEquals(intersection(fourNumbers, IntCons(4, IntNil())), IntCons(4, IntNil()))

  test("intersection: self"):
    assertEquals(intersection(twoNumbers, twoNumbers), twoNumbers)
    assertEquals(intersection(fourNumbers, fourNumbers), fourNumbers)

  test("intersection: two lists"):
    assertEquals(intersection(twoNumbers, fourNumbers), IntCons(-3, IntNil()))

  test("difference: empty list"):
    assertEquals(difference(IntNil(), fourNumbers), IntNil())
    assertEquals(difference(fourNumbers, IntNil()), fourNumbers)

  test("difference: first"):
    assertEquals(
      difference(fourNumbers, IntCons(1, IntNil())),
      IntCons(2, IntCons(-3, IntCons(4, IntNil())))
    )

  test("difference: last"):
    assertEquals(
      difference(fourNumbers, IntCons(4, IntNil())),
      IntCons(1, IntCons(2, IntCons(-3, IntNil())))
    )

  test("difference: self"):
    assertEquals(difference(twoNumbers, twoNumbers), IntNil())
    assertEquals(difference(fourNumbers, fourNumbers), IntNil())

  test("difference: pair"):
    assertEquals(
      difference(twoNumbers, fourNumbers),
      IntCons(-5, IntNil())
    )
    assertEquals(
      difference(fourNumbers, twoNumbers),
      IntCons(1, IntCons(2, IntCons(4, IntNil())))
    )

  test("minMax: empty list"):
    intercept[IllegalArgumentException]:
      minMax(IntNil())

  test("minMax: list with 2 elements"):
    assertEquals(minMax(twoNumbers), (-5, -3))

  test("minMax: list with 4 elements"):
    assertEquals(minMax(fourNumbers), (-3, 4))

  test("polishEval: addition"):
    val xs = IntCons(Add, IntCons(1, IntCons(2, IntNil())))
    assertEquals(polishEval(xs)._1, 3)

  test("polishEval: nested operations"):
    val xs = IntCons(Add, IntCons(2, IntCons(Multiply, IntCons(3, IntCons(4, IntNil())))))
    assertEquals(polishEval(xs)._1, 14)

  def assertListEquals(list1: IntList, list2: IntList): Unit =
    if list1.isEmpty && list2.isEmpty then
      // Both lists are empty, so they are equal.
      return

    if list1.isEmpty || list2.isEmpty then
      // One of the lists is empty and the other is not, so they are not equal.
      fail("One of the lists is shorter than the other.")

    if list1.head != list2.head then
      // The current elements in both lists are different.
      fail(s"List elements differ. Expected: ${list1.head}, but found: ${list2.head}.")

    // Continue the comparison with the tails of both lists.
    assertListEquals(list1.tail, list2.tail)
