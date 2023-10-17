package poly

import scala.collection.immutable.List

def findFirstEvenNumber(l: List[Int]): Option[Int] = 
    l match
        case Nil => None
        case x :: xs => if x % 2 == 0 then Some(x) else findFirstEvenNumber(xs)

def parseStringToInt(s: String): Option[Int] = 
    try
        Some(s.toInt)
    catch
        case e: NumberFormatException => None

def findSquareRoot(n: Int): Option[Double] = 
    if n < 0 then None else Some(Math.sqrt(n))

def findSquareRootFromString(s: String): Option[Double] = 
    parseStringToInt(s).flatMap(findSquareRoot)

val numberStrings: List[String] = List("1", "2", "star", "4")

val numbers = numberStrings.flatMap(parseStringToInt)
