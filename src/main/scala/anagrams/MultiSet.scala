package anagrams

import scala.annotation.tailrec

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
    @tailrec
    def subsetsWithCount(remaining: MultiSet[A], acc: List[MultiSet[A]]): List[MultiSet[A]] =
      remaining match
        case Nil => acc // Base case: no more elements, return accumulated subsets
        case (item, count) :: rest =>
          // Generate all possible additions of the current item from 0 to `count` times
          val newSubsets =
            for
              subset <- acc
              c <- 0 to count
            yield if (c > 0) ((item, c) :: subset).reverse else subset
          subsetsWithCount(rest, newSubsets)

    subsetsWithCount(set, List(Nil))

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
  def subtract(other: MultiSet[A]): MultiSet[A] = {
    // Efficiently subtract one occurrence list from another
    other.foldLeft(set) {
      case (acc, (char, count)) =>
        acc.map { case (ch, cnt) =>
          if (ch == char) (ch, cnt - count) else (ch, cnt)
        }.filter(_._2 > 0) // Filter out chars with zero or negative counts
    }
  }

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
