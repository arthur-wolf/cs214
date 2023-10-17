package poly

import poly.MyList.*
import scala.annotation.tailrec

def map[A, B](f: A => B)(l: MyList[A]): MyList[B] = 
  l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(f)(xs))

def filter[A](p: A => Boolean)(l: MyList[A]): MyList[A] = 
  l match
    case Nil         => Nil
    case Cons(x, xs) => if p(x) then Cons(x, filter(p)(xs)) else filter(p)(xs)

def foldRight[A, B](f: (A, B) => B, base: B)(l: MyList[A]): B = 
  l match
    case Nil         => base
    case Cons(x, xs) => f(x, foldRight(f, base)(xs))

def reduceRight[A](f: (A, A) => A)(l: MyList[A]): A = 
  l match
    case Nil          => throw new IllegalArgumentException("Empty list!")
    case Cons(x, Nil) => x
    case Cons(x, xs)  => f(x, reduceRight(f)(xs))

def forall[A](p: A => Boolean)(l: MyList[A]): Boolean = 
  l match
    case Nil         => true
    case Cons(x, xs) => p(x) && forall(p)(xs)

def exists[A](p: A => Boolean)(l: MyList[A]): Boolean = 
  l match
    case Nil         => false
    case Cons(x, xs) => p(x) || exists(p)(xs)

def zip[A, B](l1: MyList[A], l2: MyList[B]): MyList[(A, B)] = 
  (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons((x, y), zip(xs, ys))

def zipWith[A, B, C](op: (A, B) => C)(l1: MyList[A], l2: MyList[B]): MyList[C] = 
  (l1, l2) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(op(x, y), zipWith(op)(xs, ys))

def elementsAsStrings[A](l: MyList[A]): MyList[String] =
  l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(x.toString, elementsAsStrings(xs))

def length[A](l: MyList[A]): Int = 
  l match
    case Nil         => 0
    case Cons(_, xs) => 1 + length(xs)

def takeWhilePositive(l: MyList[Int]): MyList[Int] = 
  l match
    case Nil         => Nil
    case Cons(x, xs) => if x > 0 then Cons(x, takeWhilePositive(xs)) else Nil

def last[A](l: MyList[A]): A = 
  l match
    case Nil         => throw new IllegalArgumentException("Empty list!")
    case Cons(x, Nil) => x
    case Cons(_, xs)  => last(xs)

val capitalizeString: MyList[Char] => MyList[Char] =
  map[Char, Char](_.toUpper)

case class wordCountState(count: Int, lastWasWS: Boolean)


def wordCount(l: MyList[Char]): Int =
  foldLeft[Char, wordCountState](
    wordCountState(0, true),
    (state: wordCountState, c: Char) =>
      val cIsWS = c.isWhitespace
      val count = state.count + (if state.lastWasWS && !cIsWS then 1 else 0)
      wordCountState(count, cIsWS)
  )(l).count

def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match
  case Nil         => l2
  case Cons(x, xs) => Cons(x, append(xs, l2))

extension [A](l: MyList[A])
  def ++(that: MyList[A]): MyList[A] = append(l, that)

def flatMap[A, B](f: A => MyList[B])(l: MyList[A]): MyList[B] = 
  l match
    case Nil         => Nil
    case Cons(x, xs) => f(x) ++ flatMap(f)(xs)

def flatten[A](l: MyList[MyList[A]]): MyList[A] =
  flatMap[MyList[A], A](identity)(l)

def cross[A, B](l1: MyList[A], l2: MyList[B]): MyList[(A, B)] =
  flatMap[A, (A, B)](a => map[B, (A, B)](b => (a, b))(l2))(l1)

def allThreeLetterWords(words: MyList[String]): MyList[String] =
  filter[String](_.length == 3)(words)

def sum0(l: MyList[Int]): Int = l match
  case Nil         => 0
  case Cons(x, xs) => x + sum0(xs)

def sum1(l: MyList[Int]): Int =
  @tailrec // Uncomment this line.
  def sum(l: MyList[Int], acc: Int): Int = 
    l match
      case Nil         => acc
      case Cons(x, xs) => sum(xs, acc + x)
  sum(l, 0)

@tailrec // Uncomment this line.
def foldLeft[A, B](base: B, f: (B, A) => B)(l: MyList[A]): B = 
  l match
    case Nil         => base
    case Cons(x, xs) => foldLeft(f(base, x), f)(xs)

def sum0Fold(l: MyList[Int]): Int = 
  foldLeft[Int, Int](0, (acc, x) => acc + x)(l)

def sum1Fold(l: MyList[Int]): Int = 
  foldLeft[Int, Int](0, (acc, x) => acc + x)(l)

def reverseAppend[A](l1: MyList[A], l2: MyList[A]): MyList[A] = 
  foldLeft[A, MyList[A]](l2, (acc, x) => Cons(x, acc))(l1)

def reverse[A](l: MyList[A]): MyList[A] = reverseAppend(l, Nil)

val countEven: MyList[Int] => Int = 
  foldLeft[Int, Int](0, (acc, x) => if x % 2 == 0 then acc + 1 else acc)

val totalLength: MyList[String] => Int =
  foldLeft[String, Int](0, (acc, x) => acc + x.length)

val curriedZipWith = [A, B, C] =>
  (op: (A, B) => C) =>
    (l1: MyList[A]) =>
      (l2: MyList[B]) =>
        map[(A, B), C](t => op(t._1, t._2))(zip(l1, l2))
