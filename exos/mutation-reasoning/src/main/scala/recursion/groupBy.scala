package recursion.groupBy
def groupByForeach[T, S](f: T => S)(xs: List[T]): Map[S, List[T]] =
    var map = Map.empty[S, List[T]]
    xs.foreach(e => {
        val k = f(e)
        val v = map.getOrElse(k, Nil)
        map = map + (k -> (e :: v))
    })
    map

def groupByFoldRight[T, S](f: T => S)(xs: List[T]): Map[S, List[T]] =
    xs.foldRight(Map.empty[S, List[T]])({ (x, res) =>
        val k = f(x)
        val v = res.getOrElse(k, Nil)
        res + (k -> (x :: v))
    })
