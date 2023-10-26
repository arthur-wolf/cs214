package parallelism

import collection.parallel.CollectionConverters.IterableIsParallelizable
import parallelism.common.Task.task

object FoldReduce:

  // start reduceWithFold
  extension [A](l: List[A])
    def reduceWithFold(op: (A, A) => A): A = 
      l match
        case Nil => throw new UnsupportedOperationException("empty.reduce")
        case x :: xs => xs.foldLeft(x)(op)
  // end reduceWithFold

  // start reducePar
  extension [A](l: List[A])
    def reducePar(op: (A, A) => A): A =
      l match
        case Nil => throw new UnsupportedOperationException("Can't reduce an empty list")
        case head :: Nil => head
        case _ => 
          val (left, right) = l.splitAt(l.length / 2)
          val List(leftR, rightR) = 
            List(left, right).par.map(e => e.reducePar(op)).toList
          op(leftR,rightR)
      
  // end reducePar

  // start aggregateMapReduce
  extension [A](l: List[A])
    def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B =
      l.foldLeft(z)(seqop)
  // end aggregateMapReduce

end FoldReduce
