package org.suecarter.tablediff

import org.scalatest.FunSuite
import org.suecarter.tablediff.TableDiff._
import org.suecarter.tablediff.ReportContent._
import org.suecarter.tablediff.{DiffLocation => DL}
import TableDiffTestCases._
import scala.{List => L}

object TableDiffTestCases {
  //These are just to reduce typing on the test cases!
  val n = None

  def S[T](s: T) = if (s == n) None else Some[T](s)

  def E(l: Any, r: Any) = Left(EitherSide(S(l), S(r)))

  def D(a: Any) = Right(S(a))
  val leftReport = ReportContent(
    L(L("a", "d"), L("b", "d")),
    L(L("Col1", "Col2", "Col3"),
      L("one", "two", "three")),
    L(L(7, 5, 2),
      L(33, 1, 34)))
  val bitDifferentLeftReport = ReportContent(
    L(L("a", "d"), L("b", "d")),
    L(L("Col1", "Col2", "Col3"),
      L("one", "two", "three")),
    L(L(7, 6, 2),
      L(33, 2, 34)))
  val extraColReport = ReportContent(
    L(L("a", "d"), L("b", "d")),
    L(L("Col1", "Col2", "Col2.5", "Col3"),
      L("one", "two", "two.5", "three")),
    L(L(7, 5, 2, 2),
      L(33, 1, 2, 34)))
  val extraRowReport = ReportContent(
    L(L("a", "d"), L("z", "y"), L("b", "d")),
    L(L("Col1", "Col2", "Col3"),
      L("one", "two", "three")),
    L(L(7, 5, 2), L(2, 3, 4),
      L(33, 1, 34)))
  val changeColumnAndRowHeaderReport = ReportContent(
    L(L("a", "d"), L("b", "z")),
    L(L("Col1", "Col2", "Colz"),
      L("one", "two", "three")),
    L(L(7, 5, 2),
      L(33, 1, 34)))
  val rightReport = ReportContent(
    L(L("a", "d"), L("b1", "d")),
    L(L("Col1", "Col2", "Col4"),
      L("one", "two", "three")),
    L(L(8, 5, 23),
      L(33, 1, 34)))
  val longReport = ReportContent(
  L(L("Row1"), L("Row2"), L("Row3"), L("Row4"), L("Row5"), L("Row6")),
  L(),
  L(L(11,223), L(12,223), L(13,223), L(14,223), L(15,223), L(16,223))
  )
  val longReportRowDiffs = ReportContent(
  L(L("Row1x"), L("Row2"), L("Row3"), L("Row4x"), L("Row5x"), L("Row6"), L("Row7")),
  L(),
  L(L(11,223), L(12,223), L(13,223), L(14,223), L(15,223), L(16,223))
  )
  val unexpectedShapedReport = ReportContent(
    L(L("a"), L("b", "d", "c")),
    L(L("Col1", "Col2"),
      L("one")),
    L(L(7, 5, 2),
      L(33)))
  val emptyTestReport = ReportContent[String, String, Int](L(), L(), L())
}

class TableDiffTests extends FunSuite {
  test("matching sequences") {
    val cases: List[(List[String], List[String], List[DiffLocation[String]])] =
      L(
        (L("a"), L("b"),
        L(DL("b", n, S(0)), DL("a", S(0), n)))
        ,(L("a"), L("a"),
          L(DL("a", S(0), S(0))))
        ,(L("a", "b"), L("a"),
          L(DL("a", S(0), S(0)), DL("b", S(1), n)))
        ,(L("a", "b"), L("a", "c"),
          L(DL("a", S(0), S(0)), DL("c", n, S(1)), DL("b", S(1), n)))
        ,
        (L("a", "b", "d"), L("a", "c", "d"),
          L(DL("a", S(0), S(0)), DL("c", n, S(1)), DL("b", S(1), n),
            DL("d", S(2), S(2))))
        ,
        (L("a", "b"), L("a", "c", "d"),
          L(DL("a", S(0), S(0)), DL("c", n, S(1)), DL("b", S(1), n),
            DL("d", n, S(2))))
        ,(L("a", "d", "b", "c"), L("b", "a", "d", "a", "c"),
          L(DL("b", n, S(0)), DL("a", S(0), S(1)),
            DL("d", S(1), S(2)), DL("a", n, S(3)), DL("b", S(2), n),
            DL("c", S(3), S(4))))) ++
        {
          // Testing sparse diffs
        val i = 3000 // testing for big sequences to make sure the lcs algo can handle them
        def xs(x: String) = Stream.continually(x).take(i).toList
        def xLeftDiffs(x: String, offset: Int = 0) = xs(x).zipWithIndex.map{case(dx, ix) => DL(dx, S(ix+offset), n)}
        def xRightDiffs(x: String, offset: Int = 0) = xs(x).zipWithIndex.map{case(dx, ix) => DL(dx, n, S(ix+offset))}
        L(
          // one small match in the middle
          (xs("a") :+ "c", xs("b") :+ "c", xLeftDiffs("a") ++ xRightDiffs("b") :+ DL("c", S(i), S(i))),
          // 2 completely different seqs
          (xs("a"), xs("b"), xLeftDiffs("a") ++ xRightDiffs("b")),
          // 2 identical seqs
          (xs("a") ++ xs("b"), xs("a") ++ xs("b"), (xs("a") ++ xs("b")).zipWithIndex.map{case(dx, ix) => DL(dx, S(ix), S(ix))}),
          // mostly diff with one small match
          (xs("a") ++ L("c") ++ xs("b"), L("c"), xLeftDiffs("a") ++ xLeftDiffs("b", i+1) :+ DL("c", S(i), S(0)))
        )
      }

    cases.foreach {
      case (left, right, expected) =>
        assert(zipLongestCommonSubsequence(left/*.toIndexedSeq.zipWithIndex*/,
          right/*.toIndexedSeq.zipWithIndex*/).toSet === expected.toSet)
    }
  }

  implicit def eitherTuple2(t: (Any, Any)): Left[EitherSide[Any], Nothing] = Left(EitherSide(Some(t._1), Some(t._2)))

  test("diff reports") {
    val cases = L(
    // Tuple4(left report, right report, difference report, difference report with just the diffs in)
    {val diffReport = ReportContent(L(L(D("a"), D("d")),
              L(E("b", "b1"), D("d"))),
              L(L(D("Col1"), D("Col2"), E("Col3", n), E(n, "Col4")),
                L(D("one"), D("two"), E("three", n), E(n, "three"))),
              L(L(E(7, 8), D(5), E(2, n), E(n, 23)),
                L(D(33), D(1), E(34, n), E(n, 34))))
      (leftReport, rightReport, diffReport, Some(
        ReportContent(L(L(D("a"), D("d")),
                    L(E("b", "b1"), D("d"))),
                    L(L(D("Col1"), E("Col3", n), E(n, "Col4")),
                      L(D("one"), E("three", n), E(n, "three"))),
                    L(L(E(7, 8), E(2, n), E(n, 23)),
                      L(D(33), E(34, n), E(n, 34))))))
    },
      (leftReport, leftReport,
        ReportContent(L(L(D("a"), D("d")),
          L(D("b"), D("d"))),
          L(L(D("Col1"), D("Col2"), D("Col3")),
            L(D("one"), D("two"), D("three"))),
          L(L(D(7), D(5), D(2)),
            L(D(33), D(1), D(34)))), Some(emptyReport[ValueDiff[Any]])),
      (leftReport, bitDifferentLeftReport,
        ReportContent(
          L(L(D("a"), D("d")),
            L(D("b"), D("d"))),
          L(L(D("Col1"), D("Col2"), D("Col3")),
            L(D("one"), D("two"), D("three"))),
          L(L(D(7), E(5, 6), D(2)),
            L(D(33), E(1, 2), D(34)))), Some(
        ReportContent(
          L(L(D("a"), D("d")),
            L(D("b"), D("d"))),
          L(L(D("Col2")),
            L(D("two"))),
          L(L(E(5, 6)),
            L(E(1, 2))))
      )),
      (leftReport, changeColumnAndRowHeaderReport,
        ReportContent(
          L(L(D("a"), D("d")),
            L(D("b"), E("d", "z"))),
         L(L(D("Col1"), D("Col2"), E("Col3", "Colz")),
           L(D("one"), D("two"), D("three"))),
         L(L(D(7), D(5), D(2)),
           L(D(33), D(1), D(34)))), Some(
        ReportContent(
          L(L(D("b"), E("d", "z"))),
         L(L(E("Col3", "Colz")),
           L(D("three"))),
         L(L(D(34)))
        ))),
      (leftReport, extraColReport,
        ReportContent(L(L(D("a"), D("d")), L(D("b"), D("d"))),
          L(L(D("Col1"), D("Col2"), E(n, "Col2.5"), D("Col3")),
            L(D("one"), D("two"), E(n, "two.5"), D("three"))),
          L(L(D(7), D(5), E(n, 2), D(2)),
            L(D(33), D(1), E(n, 2), D(34)))),
        Some(ReportContent(L(L(D("a"), D("d")), L(D("b"), D("d"))),
          L(L(E(n, "Col2.5")),
            L(E(n, "two.5"))),
          L(L(E(n, 2)),
            L(E(n, 2)))))
        ),
      (leftReport, extraRowReport,
        ReportContent(
          L(L(D("a"), D("d")),
            L(E(n, "z"), E(n, "y")),
            L(D("b"), D("d"))),
          L(L(D("Col1"), D("Col2"), D("Col3")),
            L(D("one"), D("two"), D("three"))),
          L(L(D(7), D(5), D(2)),
            L(E(n, 2), E(n, 3), E(n, 4)),
            L(D(33), D(1), D(34))))
        , Some(
        ReportContent(
          L(L(E(n, "z"), E(n, "y"))),
          L(L(D("Col1"), D("Col2"), D("Col3")),
            L(D("one"), D("two"), D("three"))),
          L(L(E(n, 2), E(n, 3), E(n, 4))))
      )),
    {val diffReport =
      ReportContent(
        L(L(E("Row1", "Row1x")), L(D("Row2")), L(D("Row3")), L(E("Row4", "Row4x")), L(E("Row5", "Row5x")), L(D("Row6")), L(E(n, "Row7"))),
          L(),
          longReport.mainData.map(_.map(D(_))) :+ L(D(n), D(n)))
      (longReport, longReportRowDiffs, diffReport,
        Some(ReportContent(
                L(L(E("Row1", "Row1x")), L(E("Row4", "Row4x")), L(E("Row5", "Row5x")), L(E(n, "Row7"))),
                  L(),
                  L())
        ))}
    ,{val diffReport =
      ReportContent(L(L(E("a", n), E("d", n)),
        L(E("b", n), E("d", n))),
        L(L(E("Col1", n), E("Col2", n), E("Col3", n)),
          L(E("one", n), E("two", n), E("three", n))),
        L(L(E(7, n), E(5, n), E(2, n)),
          L(E(33, n), E(1, n), E(34, n))))
      (leftReport, emptyTestReport, diffReport, Some(diffReport))},
    {val diffReport =
      ReportContent(
        L(L(D("a"), E("d", n)),
          L(E("b", n), E("d", n)),
          L(E(n, "b"), E(n, "d"), E(n, "c"))),
        L(L(D("Col1"), E("Col2", n), E("Col3", n), E(n, "Col2")),
          L(D("one"), E("two", n), E("three", n))),
        L(L(D(7), E(5, n), E(2, n), E(n, 5), E(n, 2)),
          L(E(33, n), E(1, n), E(34, n), D(n), D(n)),
          L(E(n, 33), D(n), D(n), D(n), D(n))))
      (leftReport, unexpectedShapedReport, diffReport, Some(
        ReportContent(
              L(L(D("a"), E("d", n)),
                L(E("b", n), E("d", n)),
                L(E(n, "b"), E(n, "d"), E(n, "c"))),
              L(L(D("Col1"), E("Col2", n), E("Col3", n), E(n, "Col2"), D(n)),
                L(D("one"), E("two", n), E("three", n))),
              L(L(D(7), E(5, n), E(2, n), E(n, 5), E(n, 2)),
                L(E(33, n), E(1, n), E(34, n), D(n), D(n)),
                L(E(n, 33), D(n), D(n), D(n), D(n))))))}
      , (emptyTestReport, rightReport, ReportContent(
        rightReport.rowHeaders.map(_.map(E(n, _))),
        rightReport.columnHeaders.map(_.map(E(n, _))),
        rightReport.mainData.map(_.map(E(n, _)))), n)
      , (leftReport, emptyTestReport, ReportContent(
        leftReport.rowHeaders.map(_.map(E(_, n))),
        leftReport.columnHeaders.map(_.map(E(_, n))),
        leftReport.mainData.map(_.map(E(_, n)))), n)
      , (emptyTestReport, emptyTestReport, emptyReport[ValueDiff[Any]], Some(emptyReport[ValueDiff[Any]]))
    ,{val diffReport = ReportContent(L(L(D("a"), D("d")),
              L(E("b", n), E("d", n)),
              L(E(n, "b1"), E(n, "d"))),
              L(),
              L(L(), L(), L()))
      (ReportContent(leftReport.rowHeaders, L(L()), L(L())), ReportContent(rightReport.rowHeaders, L(L()), L(L())),
        diffReport, Some(ReportContent(L(
                      L(E("b", n), E("d", n)),
                      L(E(n, "b1"), E(n, "d"))),
                      L(),
                      L())))}

    )
    cases.foreach {
      case (l, r, p, justDiffs) =>
        val diffReport = produceReportDiff(l, r)
//        println(StringTableDiff.diffReportToString(l))
//        println(StringTableDiff.diffReportToString(r))
//        println(StringTableDiff.diffReportToString(p))
        assert(diffReport === p,
          "Expected \n" + StringTableDiff.diffReportToString(p) +
            "Actual \n" + StringTableDiff.diffReportToString(diffReport))
        val just = onlyTheDiffs(p)
//        println(StringTableDiff.diffReportToString(just))
        justDiffs.foreach(j => {
          assert(just === j,
            "Expected Only Diffs\n" + StringTableDiff.diffReportToString(j) +
              "Actual Only Diffs\n" + StringTableDiff.diffReportToString(just))
        })
    }
  }

  test("Section Diffs") {
    val cases = List(
      (L(L("A"), L("C")),
       L(L("A"), L("C")),
       L(L(D("A")), L(D("C"))))
      ,
      (L(L("A"), L("C")),
       L(L("D"), L("E")),
       L(L(E("A", "D")), L(E("C", "E"))))
      ,
      (L(L("A", "B"), L("C", "E")),
       L(L("A", "D"), L("C", "E")),
       L(L(D("A"), E("B", "D")), L(D("C"), D("E"))))
    )
    def assertReport(leftReport: ReportContent[_,_,_], rightReport: ReportContent[_,_,_], expectedDiff: ReportContent[_,_,_]) {
      val diffReport = produceReportDiff(leftReport, rightReport)
      assert(diffReport.isEquivalent(expectedDiff),
        "Expected \n" + StringTableDiff.diffReportToString(expectedDiff) +
          "Actual \n" + StringTableDiff.diffReportToString(diffReport))
    }
    cases.foreach {
      case (leftSection, rightSection, diffSection) =>
        assertReport(
          ReportContent(rowHeaders = leftSection),
          ReportContent(rowHeaders = rightSection),
          ReportContent(rowHeaders = diffSection))
        assertReport(
          ReportContent(columnHeaders = leftSection),
          ReportContent(columnHeaders = rightSection),
          ReportContent(columnHeaders = diffSection))
        assertReport(
          ReportContent(mainData = leftSection),
          ReportContent(mainData = rightSection),
          ReportContent(mainData = diffSection))
        assertReport(
          ReportContent(rowColumnHeaders = leftSection),
          ReportContent(rowColumnHeaders = rightSection),
          ReportContent(rowColumnHeaders = diffSection))
    }
  }
}
