import anagrams.*
import cs214.*

val tree = List(
    Branch(Set("a"), Nil),
    Branch(Set("bc", "cb"), List(
      Branch(Set("de", "ed"), Nil),
      Branch(Set("f"), Nil)))
)

tree.show