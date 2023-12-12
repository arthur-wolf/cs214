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
    assert(l.take(l.size -li.size).forall(e => !p(e)))
    if p(li.head) then
      return Some(li.head)
    li = li.tail
    assert(l.take(l.size -li.size).forall(e => !p(e)))
  None
}.ensuring(res =>
  res.isEmpty ||
    (p(res.get) && 
    l.contains(res.get) && 
    l.take(l.indexOf(res.get)).forall(e => !p(e)))  
)


def maxLoopList(l: List[Int]): Int =
  require(!l.isEmpty)
  var list = l.tail
  var max = l.head
  while !list.isEmpty do
    if list.head > max then
      max = list.head
    list = list.tail
  max

def maxLoopArray(a: Array[Int]): Int =
  require(!a.isEmpty)
  var idx = 1
  var max = a(0)
  while idx < a.length do
    if a(idx) > max then max = a(idx)
    idx += 1
  max    

def maxLoopArrayWithInvariant(a: Array[Int]): Int = 
  require(a.size > 0)
  def invariant(a: Array[Int], processedSize: Int, currentMax: Int): Boolean =
    a.take(processedSize).forall(e => e <= currentMax) &&
    a.take(processedSize).contains(currentMax)
  
  var idx = 1
  var max = a(0)
  assert(invariant(a, 1, max))
  while idx < a.length do
    if a(idx) > max then
      max = a(idx)
    idx += 1
    assert(invariant(a, idx, max))
  assert(invariant(a, a.length, max))
  max  


def maxLoopListWithInvariant(l: List[Int]): Int = 
  require(!l.isEmpty)
  def invariant(l: List[Int], processedSize: Int, currentMax: Int): Boolean =
    l.take(processedSize).forall(e => e <= currentMax) &&
    l.take(processedSize).contains(currentMax)
  
  var list = l.tail
  var max = l.head
  assert(invariant(l, 1, max))
  while !list.isEmpty do
    if list.head > max then
      max = list.head
    list = list.tail
    assert(invariant(l, l.size - list.size, max))
  max
