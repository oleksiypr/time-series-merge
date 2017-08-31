package op.assement.aptp

object Series {

  def apply[R: Merging](s1: Iterator[R], s2: Iterator[R]): Iterator[R] = {
    new MergeIterator[R](s1, s2)
  }

  implicit class SeriesIteratorOps[R](val a: Iterator[R]) extends AnyVal {

    /**
      * Merge operator. Usage: {{{
      *   import Series.SeriesIteratorOps
      *   val c = a :+: b
      * }}}
      * An operator is both associative and commutative: {{{
      *   a :+: b == b :+: a
      *   a :+: (b :+: c)  == (a :+: b) :+: c
      * }}}
      * @see [[MergeIterator]]
      * @param b an instance this to be merged with
      * @param ev merging strategy as an evidence parameter
      * @return merged result
      */
    def :+:(b: Iterator[R])(implicit ev: Merging[R]): Iterator[R] = Series(a, b)
  }

  trait NextCandidate[+R] {
    def isEmpty: Boolean
    def nonEmpty: Boolean = !isEmpty
  }

  trait SomeCandidate[R] extends NextCandidate[R] {
    def isEmpty: Boolean = false
    def value: R
  }

  case object NoCandidate extends NextCandidate[Nothing] {
    def isEmpty: Boolean = true
  }
  case class LeftCandidate[R](value: R) extends SomeCandidate[R]
  case class RightCandidate[R](value: R) extends  SomeCandidate[R]

  /**
    * Create merged series instance as an [[Iterator]].
    * @see [[SeriesIteratorOps]], [[Merging]]
    * @tparam R row type an evidence parameter as a merge strategy to be
    *           represented for
    * @param left row series to be merged
    * @param right row series to be merged
    */
  class MergeIterator[R: Merging](
      left:  Iterator[R],
      right: Iterator[R]
    ) extends Iterator[R] {

    private[this] val merging = implicitly[Merging[R]]
    private[this] var following: NextCandidate[R] = NoCandidate

    def hasNext: Boolean = {
      following.nonEmpty ||
      left.hasNext ||
      right.hasNext
    }

    def next(): R = following match {
      case NoCandidate =>
        if (left.hasNext && !right.hasNext) left.next() else
        if (!left.hasNext && right.hasNext) right.next()
        else merge(left.next(), right.next())

      case LeftCandidate(valueLeft) =>
        if (right.hasNext) merge(valueLeft, right.next())
        else {
          following = NoCandidate
          valueLeft
        }

      case RightCandidate(valueRight) =>
        if (left.hasNext) merge(left.next(), valueRight)
        else {
          following = NoCandidate
          valueRight
        }
    }

    private def merge(left: R, right: R): R = {
      import merging._

      if (left < right) {
        following = RightCandidate(right)
        left
      } else
      if (left > right) {
        following = LeftCandidate(left)
        right
      } else {
        following = NoCandidate
        merging.weld(left, right)
      }
    }
  }
}
