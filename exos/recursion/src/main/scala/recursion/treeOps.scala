package recursion

def treeSize(t: IntTree): Int =
  if (t == IntEmptyTree()) 0
  else 1 + treeSize(t.left) + treeSize(t.right)

def treeDepth(t: IntTree): Int =
  if (t == IntEmptyTree()) 0
  else 1 + treeDepth(t.left).max(treeDepth(t.right))

def treeSum(t: IntTree): Int =
  if (t == IntEmptyTree()) 0
  else t.value + treeSum(t.left) + treeSum(t.right)

def treeAllEven(t: IntTree): Boolean =
  if (t == IntEmptyTree()) true
  else (t.value % 2 == 0) && treeAllEven(t.left) && treeAllEven(t.right)

def treeIncrement(t: IntTree): IntTree =
  if (t == IntEmptyTree()) IntEmptyTree()
  else IntBranch(t.value + 1, treeIncrement(t.left), treeIncrement(t.right))

def treeShow(t: IntTree): String =
  if (t == IntEmptyTree()) ""
  else t.value.toString() + "\n" + treeShow(t.left) + treeShow(t.right)
  

def treeShowPostOrder(t: IntTree): String =
  if (t == IntEmptyTree()) ""
  else treeShowPostOrder(t.left) + treeShowPostOrder(t.right) + t.value.toString() + "\n"