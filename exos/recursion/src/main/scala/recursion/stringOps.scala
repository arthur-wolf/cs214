package recursion

def stringLength(s: String): Int =
  if (s.isEmpty()) 0
  else 1 + stringLength(s.tail)

def capitalizeString(s: String): String =
  if (s.isEmpty()) ""
  else s.head.toUpper.toString() + capitalizeString(s.tail)

def wordCount(s: String): Int =
  ???  

def isBlank(s: String): Boolean =
  if (s.isEmpty()) true
  else s.head.isWhitespace && isBlank(s.tail)

def caesarCipher(s: String, shift: Int): String =
  if (s.isEmpty()) ""
  else if (s.head.toInt + shift > 122) (s.head.toInt + shift - 26).toChar.toString() + caesarCipher(s.tail, shift)
  else (s.head.toInt + shift).toChar.toString() + caesarCipher(s.tail, shift)

def reverseString(s: String): String =
  if (s.isEmpty()) ""
  else reverseString(s.tail) + s.head.toString()
