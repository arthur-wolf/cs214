package recursion.trees

import scala.collection.mutable.Stack
enum Tree[T]:
  case Leaf(value: T)
  case Node(left: Tree[T], right: Tree[T])

import Tree.*
def sizeRec(t: Tree[Int]): Int =
  t match
    case Leaf(v) => 0
    case Node(l, r) => 1 + Math.max(sizeRec(l), sizeRec(r))  

def sizeLoop(t: Tree[Int]): Int =
  ???

def sumRec(t: Tree[Int]): Int =
  t match
    case Leaf(value)       => value
    case Node(left, right) => sumRec(left) + sumRec(right)

def isRightLineTree(t: Tree[Int]): Boolean =
  t match
    case Leaf(_)              => true
    case Node(Leaf(_), right) => isRightLineTree(right)
    case _                    => false

def sumRightLineTree(tr: Tree[Int]): Int =
  require(isRightLineTree(tr))
  var acc = 0
  var t = tr
  while true do
    t match
      case Leaf(value) =>
        acc += value
        return acc
      case Node(Leaf(value), right) =>
        acc += value
        t = right
      case _ => // cannot happen thanks to the require clause
        return acc
  acc

def sumRotate(tr: Tree[Int], acc: Int): Int =
  tr match
    case Leaf(value)               => acc + value
    case Node(Leaf(value), right)  => sumRotate(right, acc + value)
    case Node(Node(ll, lr), right) => sumRotate(Node(ll, Node(lr, right)), acc)

def sumLoop(t: Tree[Int]): Int =
  var sum = 0
  var toVisit = Stack(t)
  while toVisit.nonEmpty do
    toVisit.pop() match
      case Leaf(value) =>
        sum += value
      case Node(left, right) =>
        toVisit.push(right)
        toVisit.push(left)
  sum
  
def reduce[T](tr: Tree[T], f: (T, T) => T): T =
  tr match
    case Leaf(value)       => value
    case Node(left, right) => f(reduce(left, f), reduce(right, f))
trait MStackTrait[A]:
  def push(a: A): Unit
  def pop(): A
  def isEmpty: Boolean
  def size: Int
  def contains(a: A): Boolean

case class MStack[A](var l: List[A] = Nil) extends MStackTrait[A]:
  def contains(a: A): Boolean = 
    l.contains(a)
  def isEmpty: Boolean = 
    l.isEmpty
  def pop(): A = 
    val a = l.head
    l = l.tail
    a
  def push(a: A): Unit = 
    l = a :: l
  def size: Int = 
    l.size

def postOrderTraversal[T](tr: Tree[T]): List[Tree[T]] =
  var toVisit = MStack[Tree[T]]()
  toVisit.push(tr)
  var postOrderNodes: List[Tree[T]] = Nil
  while !toVisit.isEmpty do
    val n = toVisit.pop()
    postOrderNodes = n :: postOrderNodes
    n match
      case Node(left, right) =>
        toVisit.push(left)
        toVisit.push(right)
      case Leaf(_) =>
  postOrderNodes

def reduceLoop[T](tr: Tree[T], f: (T, T) => T): T =
  var cache: Map[Tree[T], T] = Map()

  for (t, idx) <- postOrderTraversal(tr).zipWithIndex do
    t match
      case Leaf(v) => cache = cache + (t -> v)
      case Node(left, right) =>
        val leftValue = cache(left)
        val rightValue = cache(right)
        cache = cache + (t -> f(leftValue, rightValue))
  cache(tr)
