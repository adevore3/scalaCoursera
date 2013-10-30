package forcomp

import common._
import scala.collection.mutable.Map

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary
  
  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def updateAcc(c: Char, charCounts: List[(Char, Int)]): List[(Char, Int)] = charCounts match {
      case x :: xs => 
        if (x._1 == c) (c, x._2 + 1) :: xs
        else x :: updateAcc(c, xs)
      case List() => List((c, 1))
    }
    
    def aux(cs: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      cs match {
        case x :: xs => aux(xs, updateAcc(x, acc))
        case List() => acc
      }
    }
    
    aux(chars, List[(Char, Int)]())
  }
  
  def insert[A](elem: A, la: List[A], f: (A, A) => Boolean): List[A] = {
    def aux(x: A, lx: List[A]): List[A] = lx match {
      case List() => List(x)
	  case l :: ls => 
	    if (f(x, l)) x :: lx 
	    else l :: aux(x, ls)  
    }
	aux(elem, la)
  }

  def insertionSort[A](la: List[A], f: (A, A) => Boolean): List[A] = {
    la match {
      case List() => List()
      case l :: ls => insert(l, insertionSort(ls, f), f)
    }
  }

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = 
    insertionSort(times(w.toLowerCase.toList), (p: (Char,Int), p2: (Char,Int)) => p._1 < p2._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.flatten.mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    var map: Map[Occurrences, List[Word]] = Map.empty
    for (w <- dictionary) {
      val o = wordOccurrences(w)
      map.get(o) match {
        case None => map.put(o, List(w))
        case Some(l) => map.put(o, w :: l)
      }
    }
    map
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case None => List()
    case Some(anagram) => anagram
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def aux(list: List[Occurrences], partialAcc: Occurrences, acc: List[Occurrences]): List[Occurrences] = list match {
	  case List() => partialAcc :: acc
	  case head :: tail => head match {
	  	case List() => aux(tail, partialAcc, acc)
	  	case h :: t => aux(tail, partialAcc ++ List(h), aux(t :: tail, partialAcc, acc))
	  }
	}
    
    def func( t: (Char, Int) ) = (for (i <- 1 to t._2) yield (t._1, i)).toList
  	
    val l = for (s <- occurrences) yield (func(s))
    
    aux(l, List(), List())
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def aux(remainder: Occurrences, remove: Occurrences): Occurrences = {
      remainder match {
        case List() => remainder
        case (xc, xi) :: remainderTail => remove match {
          case List() => (xc, xi) :: aux(remainderTail, y)
          case (yc, yi) :: removeTail =>
            if (xc == yc) {
              if (xi > yi) (xc, xi - yi) :: aux(remainderTail, removeTail)
              else aux(remainderTail, removeTail)
            } else {
              aux(remainder, removeTail)
            }
        }
      }
    }
    aux(x, y)
  }
    

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def aux(combos: List[Occurrences], occurrencesLeft: Occurrences, sentenceAcc: Sentence, acc: List[Sentence]): List[Sentence] = {
      occurrencesLeft match {
        case List() => sentenceAcc :: acc
        case _ => combos match {
	      case List() => acc
	      case occurrence :: tail => {
	        dictionaryByOccurrences.get(occurrence) match {
			  case None => aux(tail, occurrencesLeft, sentenceAcc, acc)
			  case Some(anagrams) => {
			    val newOccurrencesLeft = subtract(occurrencesLeft, occurrence) 
	            (for (a <- anagrams) 
	              yield (aux(
	            		  combinations(newOccurrencesLeft), 
	            		  newOccurrencesLeft,
	            		  a :: sentenceAcc,
	            		  acc))
	            ).flatten
			  }
	        }
	      }
	    } 
      } 
    }

    val occurrences = sentenceOccurrences(sentence)
    aux(combinations(occurrences), occurrences, List(), List())
     
  }

  
}
