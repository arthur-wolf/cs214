package find

def findAndPrint(entry: cs214.Entry, predicate: cs214.Entry => Boolean): Boolean =
  val thisFound =
    predicate(entry) 
    && { 
      println(entry.path()) 
      true 
    }

  val childrenFound = 
    if entry.isDirectory() then
      entry.hasChildren()
        && findAndPrint(entry.firstChild(), predicate)
    else false

  val nextSiblingsFound =
    entry.hasNextSibling()
      && findAndPrint(entry.nextSibling(), predicate)

  thisFound || childrenFound || nextSiblingsFound

def findAllAndPrint(entry: cs214.Entry): Boolean =
  findAndPrint(entry, _ => true)

def findByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  findAndPrint(entry, _.name() == name)

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  findAndPrint(entry, entry => (!entry.isDirectory() && entry.size() == size))

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  findAndPrint(entry, entry => (!entry.isDirectory() && entry.size() >= minSize))
  
def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  findAndPrint(entry, entry => (entry.isDirectory() && !entry.hasChildren()) || (!entry.isDirectory() && entry.size() == 0))

def howManyHoursISpentOnThisLab(): Double =
  1.0 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  val thisFound =
    entry.name() == name
      && { println(entry.path()); true }

  def childrenFound =
    entry.isDirectory()
      && entry.hasChildren()
      && findFirstByNameAndPrint(entry.firstChild(), name)

  def nextSiblingsFound =
    entry.hasNextSibling()
      && findFirstByNameAndPrint(entry.nextSibling(), name)

  thisFound || childrenFound || nextSiblingsFound
