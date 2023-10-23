package comprehensions

import collection.immutable.List
import scala.util.Random.shuffle
import scala.util.Random.nextInt

def onlyThreeLetterWords(words: List[String]): List[String] =
  for word <- words 
    if word.length() == 3 
      yield word

def louder(words: List[String]): List[String] =
  for word <- words 
    yield word.toUpperCase()

def echo(words: List[String], n: Int): List[String] =
  for word <- words
    _ <- 1 to n
      yield word

def allTogether(words: List[String], n: Int): List[String] =
  echo(louder(onlyThreeLetterWords(words)), n)

def cross[A, B](l1: List[A], l2: List[B]): List[(A, B)] =
  for a <- l1
    b <- l2
      yield (a, b)

type NodeId = Int
type DirectedEdge = (NodeId, NodeId)
type DirectedGraph = List[DirectedEdge]

def triangles(edges: DirectedGraph): List[(NodeId, NodeId, NodeId)] =
  for
    e1 <- edges

    // The first node is the smallest of the cycle.
    if (e1._1 < e1._2)

    e2 <- edges

    // The two edges are connected and
    // there exists an edge between the two other end points.
    if (e1._2 == e2._1 && edges.contains((e2._2, e1._1)))

    // The first node is the smallest of the cycle.
    if (e1._1 < e2._2 && e2._1 != e2._2)
  yield (e1._1, e1._2, e2._2)
