import anagrams.*
import cs214.*

val tree = List(
    Branch(Set("a"), Nil),
    Branch(Set("bc", "cb"), List(
      Branch(Set("de", "ed"), Nil),
      Branch(Set("f"), Nil)))
)

tree.show

object IntMultiSet:
  def from(seq: Seq[Int]): MultiSet[Int] =
    MultiSet.from(seq)(_ < _)

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