package op.assement.aptp

import org.scalatest.FunSuite
import Series._

class SeriesTest extends FunSuite {
  test("merge empty with empty") {
    assert((Nil.iterator :+: Nil.iterator).toList === Nil)
  }

  test("merge non empty with empty") {
    def s = List(("2017-08-27", 1), ("2017-08-28", 2)).iterator
    val expected = List(("2017-08-27", 1), ("2017-08-28", 2))

    assert((s :+: Nil.iterator).toList === expected)
    assert((Nil.iterator :+: s).toList === expected)
  }

  test("merge single with single") {
    def s1 = List(("2017-08-27", 1)).iterator
    def s2 = List(("2017-08-27", 2)).iterator
    def s3 = List(("2017-08-28", 3)).iterator

    assert((s1 :+: s2).toList === List(("2017-08-27", 3)))
    assert((s2 :+: s3).toList === List(("2017-08-27", 2), ("2017-08-28", 3)))
  }

  test("merge single with many") {
    def single = List(("2017-08-27", 1)).iterator
    def many = List(
      ("2017-08-27", 1),
      ("2017-08-28", 2),
      ("2017-08-29", 3)
    ).iterator

    val expected = List(
      ("2017-08-27", 2),
      ("2017-08-28", 2),
      ("2017-08-29", 3)
    )
    assert((single :+: many).toList === expected)
    assert((many :+: single).toList === expected)
  }

  test("merge edge case: same in the end") {
    def s1 = List(
      ("2017-08-27", 1),
      ("2017-09-05", 4)
    ).iterator
    
    def s2 = List(
      ("2017-08-30", 2),
      ("2017-09-05", 3)
    ).iterator

    val expected = List(
      ("2017-08-27", 1),
      ("2017-08-30", 2),
      ("2017-09-05", 7)
    )

    assert((s1 :+: s2).toList === expected)
    assert((s2 :+: s1).toList === expected)
  }

  test("merge common case") {
    def s1 = List(
      ("2017-08-27", 1),
      ("2017-08-30", 2),
      ("2017-09-02", 1),
      ("2017-09-05", 3)
    ).iterator

    def s2 = List(
      ("2017-08-27", 2),
      ("2017-08-28", 1),
      ("2017-08-29", 3),
      ("2017-08-30", 4),
      ("2017-09-05", 5)
    ).iterator

    val expected = List(
      ("2017-08-27", 3),
      ("2017-08-28", 1),
      ("2017-08-29", 3),
      ("2017-08-30", 6),
      ("2017-09-02", 1),
      ("2017-09-05", 8)
    )
    assert((s1 :+: s2).toList === expected)
    assert((s2 :+: s1).toList === expected)
  }

  test("merge operator is associative and commutative :+:") {
    import Series._

    def s1 = List(("2017-08-27", 1)).iterator
    def s2 = List(("2017-08-30", 2)).iterator
    def s3 = List(("2017-08-31", 3)).iterator

    val expected = List(
      ("2017-08-27", 1),
      ("2017-08-30", 2),
      ("2017-08-31", 3)
    )

    assert((s1 :+: s2 :+: s3).toList === expected)
    assert((s2 :+: s1 :+: s3).toList === expected)
    assert((s2 :+: s3 :+: s1).toList === expected)
    assert((s3 :+: s2 :+: s1).toList === expected)
    assert((s3 :+: s1 :+: s2).toList === expected)
    assert((s1 :+: s3 :+: s2).toList === expected)
    assert((s1 :+: (s2 :+: s3)).toList === ((s1 :+: s2) :+: s3).toList)
  }
}
