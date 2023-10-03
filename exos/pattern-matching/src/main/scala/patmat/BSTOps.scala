package patmat

import BST.*

object BSTOps:
  enum FindResult:
    case Found(value: String)
    case NotFound
  import FindResult.*

  def find(tree: BST, key: Int): FindResult =
    NotFound

  def insert(tree: BST, key: Int, value: String): BST =
    tree

  def rotateLeft(tree: BST): BST =
    tree


  def rotateRight(tree: BST): BST =
    tree

