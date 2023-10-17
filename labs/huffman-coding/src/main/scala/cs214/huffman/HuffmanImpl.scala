package cs214.huffman

trait HuffmanImpl[T] extends HuffmanBase[T]:

  // Part 1: Basics

  // start weight
  def weight(tree: CodeTree[T]): Int = 
    tree match
      case Leaf(_, w) => w
      case Fork(_, _, _, w) => w
  // end weight

  // start symbols
  def symbols(tree: CodeTree[T]): List[T] = 
    tree match
      case Leaf(s, _) => List(s)
      case Fork(_, _, s, _) => s
  // end symbols

  // start makeCodeTree
  def makeCodeTree(left: CodeTree[T], right: CodeTree[T]): CodeTree[T] =
    Fork(left, right, symbols(left) ++ symbols(right), weight(left) + weight(right))
  // end makeCodeTree

  // Part 2: Constructing Huffman trees

  // start symbolFreqs
  def symbolFreqs(symbols: List[T]): List[(T, Int)] = 
    symbols.groupBy(identity).view.mapValues(_.size).toList
  // end symbolFreqs

  // start makeOrderedLeafList
  def makeOrderedLeafList(freqs: List[(T, Int)]): List[Leaf[T]] = 
    freqs.sortWith(_._2 < _._2).map((s, w) => Leaf(s, w))
  // end makeOrderedLeafList

  // start isSingleton
  def isSingleton(trees: List[CodeTree[T]]): Boolean = 
    trees.size == 1
  // end isSingleton

  // start combine
  def combine(trees: List[CodeTree[T]]): List[CodeTree[T]] = 
    trees match
      case Nil => Nil
      case t :: Nil => trees
      case t1 :: t2 :: ts => (makeCodeTree(t1, t2) :: ts).sortWith(weight(_) < weight(_))
  // end combine

  // start until
  def until(isDone: List[CodeTree[T]] => Boolean, merge: List[CodeTree[T]] => List[CodeTree[T]])(trees: List[CodeTree[T]]): List[CodeTree[T]] = 
    if isDone(trees) then trees
    else until(isDone, merge)(merge(trees))
  // end until

  // start createCodeTree
  def createCodeTree(symbols: List[T]): CodeTree[T] = 
    val leafs = makeOrderedLeafList(symbolFreqs(symbols))
    until(isSingleton, combine)(leafs).head
  // end createCodeTree

  // Part 3: Decoding
  // Reminder: type Bit = Int

  // start decodeOne
  def decodeOne(tree: CodeTree[T], bits: List[Bit]): Option[(T, List[Bit])] = 
    tree match
      case Leaf(s, _) => Some((s, bits))
      case Fork(left, right, _, _) => 
        bits match
          case Nil => None
          case b :: bs => 
            if b == 0 then decodeOne(left, bs)
            else decodeOne(right, bs)
  // end decodeOne

  // start decode
  def decode(tree: CodeTree[T], bits: List[Bit]): List[T] = {
    @annotation.tailrec
    def decodeAcc(tree: CodeTree[T], bits: List[Bit], acc: List[T]): List[T] = 
      bits match
        case Nil => acc.reverse
        case _ => decodeOne(tree, bits) match
          case None => acc.reverse
          case Some((s, bs)) => decodeAcc(tree, bs, s :: acc)

    decodeAcc(tree, bits, Nil)
  }
// end decode

  // Part 4a: Encoding using Huffman tree

  // start encode
  def encode(tree: CodeTree[T])(text: List[T]): List[Bit] =
    text.flatMap(s => encodeOne(tree)(s))
  // end encode

  def encodeOne(tree: CodeTree[T])(symbol: T): List[Bit] = 
    tree match
      case Leaf(s, _) => if s == symbol then Nil else throw new Exception("Symbol not found")
      case Fork(left, right, _, _) => 
        if symbols(left).contains(symbol) then 0 :: encodeOne(left)(symbol)
        else if symbols(right).contains(symbol) then 1 :: encodeOne(right)(symbol)
        else throw new Exception("Symbol not found")

  // Part 4b: Encoding using code table

  // Reminder: type CodeTable = List[(T, List[Bit])]

  // start codeBits
  def codeBits(table: CodeTable)(symbol: T): List[Bit] = 
    table.find(_._1 == symbol) match
      case None => Nil
      case Some((_, bits)) => bits
  // end codeBits

  // start convert
  def convert(tree: CodeTree[T]): CodeTable = 
    tree match
      case Leaf(s, _) => List((s, Nil))
      case Fork(left, right, _, _) => 
        convert(left).map((s, bs) => (s, 0 :: bs)) ++ convert(right).map((s, bs) => (s, 1 :: bs))
  // end convert

  // start mergeCodeTables
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = 
    a.map((s, bs) => (s, 0 :: bs)) ++ b.map((s, bs) => (s, 1 :: bs))
  // end mergeCodeTables

  // start quickEncode
  def quickEncode(tree: CodeTree[T])(text: List[T]): List[Bit] = 
    val table = convert(tree)
    text.flatMap(codeBits(table))
  // end quickEncode
