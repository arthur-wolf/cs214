package hoare

import recursion.trees.Tree
import recursion.trees.Tree.*
import recursion.trees.MStack
import recursion.trees.MStackTrait
def abs(x: Int): Int = {
  if x < 0 then -x else x
} ensuring (res =>
  res >= 0
)
def find(l: List[Int], p: Int => Boolean): Option[Int] = {
  var li = l
  while !li.isEmpty do
    assert(true)
    if p(li.head) then
      return Some(li.head)
    li = li.tail
    assert(true)
  None
}
  .ensuring(res => ???)
def maxLoopList(l: List[Int]): Int =
  require(!l.isEmpty)
  ???
def maxLoopArray(a: Array[Int]): Int =
  require(!a.isEmpty)
  ???
def maxLoopArrayWithInvariant(a: Array[Int]): Int =
  require(a.size > 0)
  ???
def maxLoopListWithInvariant(l: List[Int]): Int =
  require(!l.isEmpty)
  ???
