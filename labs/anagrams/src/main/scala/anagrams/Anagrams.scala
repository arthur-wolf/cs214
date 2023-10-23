package anagrams
import java.text.Normalizer

object Anagrams:

  /** A `word` is simply a `String`. */
  type Word = String

  /** A `Sentence` is a `List` of words. */
  type Sentence = List[Word]

  /** A multiset (set with repeated elements) can be represented as a sorted
    * list of elements, each with its multiplicity (the number of time it
    * appears in the multiset).
    */
  type MultiSet[+A] = List[(A, Int)]

  /** An `OccurrenceList` is a multiset of characters: a `List` of pairs of
    * characters and positive integers indicating how many times the character
    * appears. This list is sorted alphabetically w.r.t. to the character in
    * each pair. All characters in the occurrence list are lowercase.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * List(('a', 1), ('e', 1), ('t', 1))
    *
    * Incidentally, so do the words "ate" and "tea".
    */
  type OccurrenceList = List[(Char, Int)]

  /** A `Dictionary` is a mapping from occurrence lists to corresponding words.
    *
    * For example, if the original word list contains the entries `ate`, `eat`,
    * `tea`, then the resulting `Dictionary` will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    */
  type Dictionary = Map[OccurrenceList, List[Word]]

  /** Removes diacritics and all non-alphabetic characters from `s`. */
  def normalizeString(str: String): String = {
    Normalizer
      .normalize(str, Normalizer.Form.NFD)
      .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
      .replaceAll("[^a-zA-Z]+", "")
      .toLowerCase
  } ensuring (_.forall(c => 'a' <= c && c <= 'z'))

  /** Normalizes a string `str` and converts it into its occurrence list. */
  def computeOccurrenceList(str: Word): OccurrenceList =
    for (c <- normalizeString(str).toList.distinct.sorted)
      yield (c, normalizeString(str).count(_ == c))

  /** Converts a sentence `s` into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): OccurrenceList =
    computeOccurrenceList(s.mkString)

  /** `createDictionary` constructs a `Map` from occurrence lists to a sequence
    * of all the words that have that occurrence count. This map makes it easy
    * to obtain all the anagrams of a word given its occurrence list.
    */
  def createDictionary(wordlist: List[String]): Dictionary =
    wordlist
      .groupBy(computeOccurrenceList)
      .map((k, v) => (k, v.distinct))

  /** Returns all anagrams of a given word. `word` must appear in one of the
    * value lists in `dict`.
    */
  def wordAnagrams(dict: Dictionary, word: Word): List[Word] =
    dict(computeOccurrenceList(word))

  /** Returns the list of all subsets of a given `MultiSet`.
    *
    * This includes the multiset itself, i.e. `List(('k', 1), ('o', 1))` is a
    * subset of `List(('k', 1), ('o', 1))`. It also include the empty multiset
    * `List()`.
    *
    * For example, the subsets of `List(('a', 2), ('b', 2))` are:
    *
    * List(), List(('a', 1)), List(('a', 2)), List(('b', 1)), List(('b', 2)),
    * List(('a', 1), ('b', 1)), List(('a', 2), ('b', 1)), List(('a', 1), ('b',
    * 2)), List(('a', 2), ('b', 2))
    *
    * Note that the order in which subsets are returned does not matter -- the
    * subsets in the example above could have been displayed in any other order.
    */
  def subsets[A](multiset: MultiSet[A]): List[MultiSet[A]] =
    multiset match
      case Nil => List(Nil)
      case (a, n) :: xs =>
        val rest = subsets(xs)
        rest ++ (for
          i <- 1 to n
          s <- rest
        yield (a, i) :: s)

  /** Subtracts multiset `y` from multiset `x`.
    *
    * `y` must be a subset of `x`: if an element `c` appears `n` times in `y`,
    * it must also appear at least `n` times in `x`.
    *
    * Note: the resulting value must be a valid multiset: it must be sorted and
    * have no zero entries.
    */
  def subtract[A](x: MultiSet[A], y: MultiSet[A]): MultiSet[A] =
    (x, y) match
      case (Nil, _) => Nil
      case (_, Nil) => x
      case ((a, n) :: xs, (b, m) :: ys) =>
        if a == b then
          if n > m then (a, n - m) :: subtract(xs, ys)
          else subtract(xs, ys)
        else (a, n) :: subtract(xs, y)

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its
    * anagrams, given a typical English wordlist:
    *
    *   - List(en, as, my),
    *   - List(en, my, as),
    *   - List(man, yes),
    *   - List(men, say),
    *   - List(as, en, my),
    *   - List(as, my, en),
    *   - List(sane, my),
    *   - List(Sean, my),
    *   - List(my, en, as),
    *   - List(my, as, en),
    *   - List(my, sane),
    *   - List(my, Sean),
    *   - List(say, men),
    *   - List(yes, man)
    *
    * The different sentences do not have to be output in the order shown above:
    * any order is fine as long as all the anagrams are there. Every word
    * returned has to exist in the dictionary.
    *
    * Note: If the words of the sentence are in the dictionary, then the
    * sentence is an anagram of itself, and it must appear in the result.
    *
    * Note: An empty sentence has one anagram: itself.
    */

  def anagrams(dict: Dictionary, occurrences: OccurrenceList): List[Sentence] =
    if occurrences.isEmpty then
      List(Nil)
    else
      for
        subset <- subsets(occurrences)
        word <- dict.getOrElse(subset, Nil)
        rest <- anagrams(dict, subtract(occurrences, computeOccurrenceList(word)))
      yield word :: rest

  def sentenceAnagrams(dict: Dictionary, sentence: Sentence): List[Sentence] =
    anagrams(dict, sentenceOccurrences(sentence))

  // start timeSpent
  def howManyHoursISpentOnThisLab(): Double =
    4 // in hours, so put 3.5 here if you spent 3 hours and a half on the lab
  // end timeSpent
