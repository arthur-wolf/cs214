package recursion.mergeSort
def sort(original: Array[Int]): Array[Int] =
  val n = original.length

  var a = original
  var b = new Array[Int](n)
  var width = 1

  while width < n do
    var i = 0
    while i < n do
      merge(a, b, i, width)
      i += 2 * width
    b.copyToArray(a)
    width *= 2

  a

def merge(a: Array[Int], b: Array[Int], indexLeft: Int, width: Int) =
  var i = indexLeft
  var j = indexLeft + width
  var endIndex = indexLeft + 2 * width

  for k <- indexLeft until endIndex do
    if j >= endIndex || (i < indexLeft + width && a(i) < a(j)) then
      b(k) = a(i)
      i += 1
    else
      b(k) = a(j)
      j += 1
