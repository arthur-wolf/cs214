package patmat

import BST.*

object BSTOps:
  enum FindResult:
    case Found(value: String)
    case NotFound
  import FindResult.*

  def find(tree: BST, key: Int): FindResult =
    tree match
      case Leaf => NotFound
      case Branch(k, v, l, r) =>
        if key == k then Found(v)
        else if key < k then find(l, key)
        else find(r, key)
    

  def insert(tree: BST, key: Int, value: String): BST =
    tree match
      case Leaf => Branch(key, value, Leaf, Leaf)
      case Branch(k, v, l, r) =>
        if key == k then Branch(k, value, l, r)
        else if key < k then Branch(k, v, insert(l, key, value), r)
        else Branch(k, v, l, insert(r, key, value))
    
  def rotateLeft(tree: BST): BST =
    tree match
      case Leaf => Leaf
      case Branch(k, v, l, r) =>
        r match
          case Leaf => tree
          case Branch(k2, v2, l2, r2) =>
            Branch(k2, v2, Branch(k, v, l, l2), r2)
    


  def rotateRight(tree: BST): BST =
    tree match
      case Leaf => Leaf
      case Branch(k, v, l, r) =>
        l match
          case Leaf => tree
          case Branch(k2, v2, l2, r2) =>
            Branch(k2, v2, l2, Branch(k, v, r2, r))

