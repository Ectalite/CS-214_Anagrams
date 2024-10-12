import anagrams.*
import cs214.*

object MultiSets:
  object CharMultiSet:
    def from(str: String): MultiSet[Char] =
      MultiSet.from(normalizeString(str))(_ < _)
  object IntMultiSet:
    def from(seq: Seq[Int]): MultiSet[Int] =
      MultiSet.from(seq)(_ < _)
  object DoubleMultiSet:
    def from(seq: Seq[Double]): MultiSet[Double] =
      MultiSet.from(seq)(_ < _)
  object ListIntMultiSet:
    def from(seq: Seq[List[Int]]): MultiSet[List[Int]] =
      MultiSet.from(seq)(_.sum < _.sum) // Arbitrary comparison function
import MultiSets.*

val tree = List(
    Branch(Set("a"), Nil),
    Branch(Set("bc", "cb"), List(
      Branch(Set("de", "ed"), Nil),
      Branch(Set("f"), Nil)))
)

tree.show

val seq = Seq(1, 1, 2, 2, 3)

val a = seq.sortWith((x,y) => x > y)
seq.groupBy(_.intValue).map((a, b) => (a,b.size)).toList

val set1 = IntMultiSet.from(Seq(1, 1, 2, 2, 3))
set1.subsets

set1.groupBy(identity).map((a, b) => (a,b.size)).toList

val b = (for
  item <- set1
  _ <- 0 until item._2
yield item._1 )

val c = (for
  i <- b.indices
yield b.combinations(i).toList).flatten

val d= (
  for 
    item <- c
  yield item.groupBy(identity).map((a, b) => (a,b.size)).toList
  )
d ++ List(set1)

val dico = createDictionary(loadWordlist("en-debian"))
val occurrences = sentenceOccurrences(List("Linux", "rulez"))

def anagrams(dict: Dictionary, occurrences: OccurrenceList): AnagramsTree =
  (
    for
      itemDico <- dico
      if itemDico._1.subtract(occurrences).isEmpty
    yield Branch(itemDico._2, anagrams(dict, occurrences.subtract(itemDico._1)))
  ).toList
  
anagrams(dico, occurrences).show
                

