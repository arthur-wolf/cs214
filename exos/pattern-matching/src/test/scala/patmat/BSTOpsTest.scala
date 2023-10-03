import patmat.*

import BST.*
import BSTOps.*
import FindResult.*

class BSTOpsTest extends munit.FunSuite:
  test("find: singleton tree"):
    assertEquals(find(Branch(0, "a", Leaf, Leaf), 0), Found("a"))
    assertEquals(find(Branch(0, "a", Leaf, Leaf), 1), NotFound)

  test("find: simple tree"):
    val tree = Branch(0, "b", Branch(-1, "a", Leaf, Leaf), Branch(2, "d", Branch(1, "c", Leaf, Leaf), Branch(3, "e", Leaf, Leaf)))
    assertEquals(find(tree, -1), Found("a"))
    assertEquals(find(tree, 0), Found("b"))
    assertEquals(find(tree, 1), Found("c"))
    assertEquals(find(tree, 2), Found("d"))
    assertEquals(find(tree, 3), Found("e"))
    assertEquals(find(tree, 4), NotFound)
    assertEquals(find(tree, -2), NotFound)

  test("insert: singleton tree"):
    val tree = Branch(0, "a", Leaf, Leaf)
    assertEquals(insert(tree, 1, "b"), Branch(0, "a", Leaf, Branch(1, "b", Leaf, Leaf)))
    assertEquals(insert(tree, -1, "b"), Branch(0, "a", Branch(-1, "b", Leaf, Leaf), Leaf))
    assertEquals(insert(tree, 0, "b"), Branch(0, "b", Leaf, Leaf))

