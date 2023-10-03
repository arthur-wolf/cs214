package patmat

enum BST:
  case Leaf
  case Branch(k: Int, v: String, l: BST, r: BST)

  def isLeaf: Boolean = this match
    case Leaf => true
    case _: Branch => false

  def key: Int = this match
    case Leaf => throw RuntimeException("key of a leaf")
    case Branch(k, v, l, r) => k

  def value: String = this match
    case Leaf => throw RuntimeException("value of a leaf")
    case Branch(k, v, l, r) => v

  def left: BST = this match
    case Leaf => throw RuntimeException("left of a leaf")
    case Branch(k, v, l, r) => l

  def right: BST = this match
    case Leaf => throw RuntimeException("right of a leaf")
    case Branch(k, v, l, r) => r

