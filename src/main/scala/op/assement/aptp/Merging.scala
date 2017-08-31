package op.assement.aptp

/**
  * Merging strategy.
  * @tparam S type of items to be merged
  */
trait Merging[S] extends Ordering[S] {

  val ordering: Ordering[S]

  /**
    * In order to merge we should be able to compare items (with `ordering`
    * value) and if one item is not "less" and not "more" then another we
    * should "weld" them.
    * @param a first parameter to be weld
    * @param b second parameter to be weld
    * @return welded result
    */
  def weld(a: S, b: S): S

  def compare(x: S, y: S): Int = ordering.compare(x, y)
}

object Merging {
  type RowSeries = (String, Int)

  implicit object RowSeriesMerging extends Merging[RowSeries] {
    val ordering: Ordering[RowSeries] = Ordering.by(_._1)
    def weld(a: RowSeries, b: RowSeries): RowSeries = (a._1, a._2 + b._2)
  }
}
