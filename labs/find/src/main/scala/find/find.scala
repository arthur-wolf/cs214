package find

def findAllAndPrint(entry: cs214.Entry): Boolean =
  if (entry.isDirectory()) {

    println(entry.path())

    if (entry.hasChildren()) {
      findAllAndPrint(entry.firstChild())
    }

    if (entry.hasNextSibling()) {
      findAllAndPrint(entry.nextSibling())
    }

  } else {

    println(entry.path())

    if (entry.hasNextSibling()) {
      findAllAndPrint(entry.nextSibling())
    }
  }
  true

def findByNameAndPrint(entry : cs214.Entry, name: String): Boolean =
  var found = false
  if (entry.isDirectory()) {
    if (entry.name().contains(name)) {
      println(entry.path())
      found = true
    }
    if (entry.hasChildren()) {
      found = findByNameAndPrint(entry.firstChild(), name) || found
    }
    if (entry.hasNextSibling()) {
      found = findByNameAndPrint(entry.nextSibling(), name) || found
    }
  } else {
    if (entry.name().contains(name)) {
      println(entry.path())
      found = true
    }
    if (entry.hasNextSibling()) {
      found = findByNameAndPrint(entry.nextSibling(), name) || found
    }
  }
  found

def findBySizeEqAndPrint(entry: cs214.Entry, size: Long): Boolean =
  var found = false
  if (entry.isDirectory()) {
    if (entry.hasChildren()) {
      found = findBySizeEqAndPrint(entry.firstChild(), size) || found
    }
    if (entry.hasNextSibling()) {
      found = findBySizeEqAndPrint(entry.nextSibling(), size) || found
    }
  } else {
    if (entry.size() == size) {
      println(entry.path())
      found = true
    }
    if (entry.hasNextSibling()) {
      found = findBySizeEqAndPrint(entry.nextSibling(), size) || found
    }
  }
  found

def findBySizeGeAndPrint(entry: cs214.Entry, minSize: Long): Boolean =
  var found = false
  if (entry.isDirectory()) {
    if (entry.hasChildren()) {
      found = findBySizeGeAndPrint(entry.firstChild(), minSize) || found
    }
    if (entry.hasNextSibling()) {
      found = findBySizeGeAndPrint(entry.nextSibling(), minSize) || found
    }
  } else {
    if (entry.size() >= minSize) {
      println(entry.path())
      found = true
    }
    if (entry.hasNextSibling()) {
      found = findBySizeGeAndPrint(entry.nextSibling(), minSize) || found
    }
  }
  found

def findEmptyAndPrint(entry: cs214.Entry): Boolean =
  var found = false
  if (entry.isDirectory()) {
    if (entry.hasChildren()) {
      found = findEmptyAndPrint(entry.firstChild()) || found
    } else {
      println(entry.path())
      found = true
    }

    if (entry.hasNextSibling()) {
      found = findEmptyAndPrint(entry.nextSibling()) || found
    }
  } else {
    if (entry.size() == 0) {
      println(entry.path())
      found = true
    }
    if (entry.hasNextSibling()) {
      found = findEmptyAndPrint(entry.nextSibling()) || found
    }
  }
  found

def howManyHoursISpentOnThisLab(): Double =
  3 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab

///////////////////////////////
// The following is optional //
///////////////////////////////

def findFirstByNameAndPrint(entry: cs214.Entry, name: String): Boolean =
  true
