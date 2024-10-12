package anagrams

/** A multiset (also known as a bag) is a generalization of a set that allows
  * for multiple occurrences of the same element. Elements of the set should be
  * represented using a unique _canonical_ representation.
  *
  * Two multisets containing the same occurrences of the same elements should be
  * equal using default scala equality:
  * {{{
  * // Given that both m1 and m2 contain elements {a, a, b, b, b}
  * m1 == m2 // true
  * }}}
  */
type MultiSet[+A] = List[(A, Int)] //Recommended

extension [A](set: MultiSet[A])
  /** Returns the list of all subsets of a given MultiSet
    *
    * This always includes the multiset itself, i.e. `set.subsets.contains(set) // true`
   *  and the empty multiset.
    *
    * For example, the subsets of `{a, b, c}` should be
   * `({}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c})`
    *
    * Note that the order in which subsets are returned does not matter.
    * However, the elements in each subset must remain canonical.
    */
  def subsets: List[MultiSet[A]] =
    def subsetsWithCount(remaining: MultiSet[A]): List[MultiSet[A]] = remaining match {
      case Nil => List(Nil) // Base case: only one subset of the empty set
      case (item, count) :: rest =>
        // Generate all subsets for the rest of the set
        val restSubsets = subsetsWithCount(rest)
        // For each subset, include the current item 0 to `count` times
        (for {
          subset <- restSubsets
          c <- 0 to count
        } yield if (c > 0) (item, c) :: subset else subset)
    }
    subsetsWithCount(set)
  
  /** Subtracts multiset `other` from this multiset
    *
    * For example, `{1, 2, 2, 2, 3, 4, 4} - {1, 2, 4}` should be `{2, 2, 3, 4}`
    *
    * Remember that `MultiSet[+A]` is a generic type, hence your solution can't
    * use any kind of ordering.
    *
    * Note: the resulting set must be a valid multiset: it must be canonical and
    * have no zero entries.
    */
  def subtract(other: MultiSet[A]): MultiSet[A] =
    for
      item <- set
      amountInOther <- other.find((a, _) => a == item._1).map((_, amount) => item._2 - amount).orElse(Option(item._2))
      if amountInOther > 0
    yield (item._1, amountInOther)

object MultiSet:

  /** Creates a MultiSet from a given sequence, using the given comparison
    * function.
    *
    * @param lt
    *   the comparison function which tests whether its first argument precedes
    *   its second argument in the desired ordering.
    * @see
    *   https://dotty.epfl.ch/api/scala/collection/SeqOps.html#sortWith
    */
  def from[A](seq: Seq[A])(lt: (A, A) => Boolean): MultiSet[A] =
    seq.sortWith(lt).groupBy(identity).map((a, b) => (a,b.size)).toList
