package poly

import poly.MyList.*

val compose = [A, B, C] => (f: B => C, g: A => B) => (a: A) => f(g(a))

val id = [A] => (x: A) => x

def andLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean = a => f(a) && g(a)
def orLifter[A](f: A => Boolean, g: A => Boolean): A => Boolean = a => f(a) || g(a)
def sumLifter[A](f: A => Int, g: A => Int): A => Int = a => f(a) + g(a)
def listConcatLifter[A, B](f: A => MyList[B], g: A => MyList[B]): A => MyList[B] = a => f(a) ++ g(a)

def binaryLifter[A, B, C](op: (B, B) => C)(f: A => B, g: A => B): A => C = 
    a => op(f(a), g(a))

val andLifter1 = 
    [A] => (f: A => Boolean, g: A => Boolean) => (a: A) => f(a) && g(a)
val orLifter1 = 
    [A] => (f: A => Boolean, g: A => Boolean) => (a: A) => f(a) || g(a)
val sumLifter1 = 
    [A] => (f: A => Int, g: A => Int) => (a: A) => f(a) + g(a)
val listConcatLifter1 = 
    [A, B] => (f: A => MyList[B], g: A => MyList[B]) => (a: A) => f(a) ++ g(a)
