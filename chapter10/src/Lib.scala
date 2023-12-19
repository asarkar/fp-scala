object Lib:
  case class Interval(ordered: Boolean, min: Int, max: Int)

  /*
  Exercise 10.9: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
  ---
  1. By defining Monoid[Option[Interval]] instead of Monoid[Interval], we can define empty as None.
  2. The merged sequence is ordered when both inputs are ordered and the max value on the left is
     less than or equal to the min value on the right.
     We also compute a new max for the overall sequence.
   */
  val orderedMonoid: Monoid[Option[Interval]] = new:
    def combine(oa1: Option[Interval], oa2: Option[Interval]) =
      (oa1, oa2) match
        case (Some(a1), Some(a2)) =>
          Some(Interval(a1.ordered && a2.ordered && a1.max <= a2.min, a1.min, a2.max))
        case (x, None) => x
        case (None, x) => x
    val empty = None

  def isOrdered(xs: IndexedSeq[Int]): Boolean =
    Monoid
      .foldMapV(xs)(i => Some(Interval(true, i, i)))(using orderedMonoid)
      .map(_.ordered)
      .getOrElse(true)

  /*
  Exercise 10.18: A bag is like a set, except it's represented by a map that contains one entry per element,
  with that element as the key and the value under that key as the number of times the element appears in the bag.

  Use monoids to compute a bag from an IndexedSeq.
   */
  import MonoidInstances.{mapMergeMonoid, intAddition}
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    Monoid
      .foldMapV(as)(x => Map(x -> 1))
