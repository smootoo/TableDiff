package org.suecarter.tablediff

import org.scalatest.FunSuite
import scala.util.Properties
import scala.{List => L}
import ReportContent._
import TableDiffUtilTests.diffed
import org.suecarter.tablediff.TableDiff.ValueDiff

class TableDiffUtilTests extends FunSuite {
  test("forward filling header sections") {
    implicit def stringToReportSection(s: String):ReportSection[String] = s.stripMargin('^').split("\n").map(x => x.toCharArray.toSeq.map(_.toString))
    val cases: List[(ReportSection[Any], ReportSection[Any])] = List(
("""
^ab
^ c
""","""
^ab
^ac
"""),

("""
^ab
^d
""","""
^ab
^d
"""),

("""
^ab
^ c
^x
^
^yc
^z
^ e
^
^ d
""","""
^ab
^ac
^x
^x
^yc
^z
^ze
^ze
^zd
"""),

("""
^abcdef
^   ghj
^     k
^   lmk
^     n
^p   mk
^nmopqr
^     s
^rstuv
^q
^ x
""","""
^abcdef
^abcghj
^abcghk
^abclmk
^abclmn
^p   mk
^nmopqr
^nmopqs
^rstuv
^q
^qx
"""),

      (L(L("a", "b"),
         L("")),
       L(L("a", "b"),
         L("a", "b"))),

      (L(L("a", "b"),
         L()),
       L(L("a", "b"),
         L("a", "b"))),

      (L(L()),L(L())),
      (L(),L())
    )

    cases.foreach {
      case (l, r) =>
        assert(fillSectionHeaders(l) === r)
        import StringTableDiff._
        assert(diffReportToString(ReportContent(rowHeaders = diffed(l))) ===
          diffReportToString(ReportContent(rowHeaders = removeHeaderDuplicates(diffed(r)))))
    }
  }

  test("forward filling of tables") {
    import TableDiffUtilTests.stringToReport
    val cases: List[(ReportContent[String, String, String], ReportContent[String, String, String])] = List(
("""
^ab|a x
^  |bc
^-----
^a |d
^ b| f
""","""
^ab|aax
^  |bc
^-----
^a |d
^ab| f
"""),
("""
^-----
^|d
^b| f
""","""
^-----
^|d
^b| f
"""),
("""
^ab|c  f
^  |hghj
^-------
^ab|cghk
^  |clmk
^  |clmn
^p |  mk
^nm|opqr
^  |opqs
^rs|tuv
^q |
^ x|
""","""
^ab|cccf
^  |hghj
^-------
^ab|cghk
^ab|clmk
^ab|clmn
^p |  mk
^nm|opqr
^nm|opqs
^rs|tuv
^q |
^qx|
"""))

    cases.foreach {
      case (l, r) =>
        import StringTableDiff._
        import TableDiff._
//        println(diffReportToString(fillForwardReportHeaders(diffed(l))))
//        println(diffReportToString(fillForwardReportHeaders(diffed(r))))
//        println(diffReportToString(onlyTheDiffs(produceReportDiff(l,r))))
        assert(onlyTheDiffs(produceReportDiff(l,r)) === emptyReport)
//        println(diffReportToString(fillForwardReportHeaders(diffed(l))))
//        println(diffReportToString(r))
        assert(diffReportToString(fillForwardReportHeaders(diffed(l))) === diffReportToString(diffed(r)))
        assert(diffReportToString(removeDuplicateReportHeaders(fillForwardReportHeaders(diffed(l)))) === diffReportToString(diffed(l)))
    }
  }

  test("Fill forward on diff only results") {
    import TableDiffUtilTests.stringToReport
    val cases: List[(ReportContent[String, String, String],
      ReportContent[String, String, String],
      ReportContent[ValueDiff[String], ValueDiff[String], ValueDiff[String]])] = List(
("""
^  |a f
^nm|bcg
^------
^a |d h
^ b| fy
""","""
^  |a f
^nm|bcg
^------
^a |d h
^ b| xy
""",
  ReportContent(List(List(Right(Some("a")),Right(Some("b")))),
    List(List(Right(Some("a"))), List(Right(Some("c")))),
    List(List(Left(EitherSide(Some("f"),Some("x"))))),
    List(List(Right(Some(" ")),Right(None)),List(Right(Some("n")),Right(Some("m"))))))
      ,
("""
^  |a g
^nm|bch
^------
^a |d
^ b| f
""","""
^  |axg
^nm|bch
^------
^a |d
^ab| f
""",
  ReportContent(List(),List(List(Left(EitherSide(Some("a"),Some("x")))), List(Right(Some("c")))),
    List(List(Right(None))),
    List()))
    ,
("""
^  |a g
^nm|bch
^------
^a |d
^ b| f
""","""
^  |axg
^nx|bch
^------
^a |d
^ab| f
""",
        ReportContent(List(),List(List(Left(EitherSide(Some("a"),Some("x")))), List(Right(Some("c")))),
          List(List(Right(None))),
          List(List(Right(Some(" ")),Right(None)),List(Right(Some("n")),Left(EitherSide(Some("m"),Some("x")))))))
      ,
("""
^  |a g
^nm|bch
^------
^a |d
^ b| f
""","""
^  |a g
^nm|bch
^------
^ax|d
^ab| f
""",
  ReportContent(List(List(Right(Some("a")),Left(EitherSide(Some(" "), Some("x"))))),List(),
    List(),
    List(List(Right(Some(" ")),Right(None)),List(Right(Some("n")),Right(Some("m")))))
    )
    )
    cases.foreach {
      case (l, r, d) =>
        import StringTableDiff._
        import TableDiff._
//        println(diffReportToString(diffed(l)))
//        println(diffReportToString(diffed(r)))
//        println(diffReportToString(d))
//        println(diffReportToString(produceReportDiff(l,r)))
        val onlyDiffs = onlyTheDiffs(produceReportDiff(l,r))
//        println(diffReportToString(onlyDiffs))
        assert(onlyDiffs === d)
    }
  }
  test("read chunk env var") {
    assert(TableDiff.readChunkEnvVar() == TableDiff.diffChunkSize)
    println(s"testing invalid ${TableDiff.chunkEnvVarName}")
    assert(TableDiff.readChunkEnvVar(Some("xxx")) == TableDiff.diffChunkSize)
  }
}

object TableDiffUtilTests {
  def diffCell(x: Any): ValueDiff[Any] =  Right(x.toString.trim match {
    case "" => None
    case y => Some(y)
  })
  def diffed(x: ReportSection[Any]): ReportSection[ValueDiff[Any]] = x.map(_.map(diffCell))
  def diffed(x: ReportContent[Any, Any, Any]) = x.copy(rowHeaders = x.rowHeaders.map(_.map(diffCell)),
                                               columnHeaders = x.columnHeaders.map(_.map(diffCell)),
                                               mainData = x.mainData.map(_.map(diffCell)),
                                               rowColumnHeaders = x.rowColumnHeaders.map(_.map(diffCell)))
  implicit def stringToReport(s: String): ReportContent[String, String, String] = {
    def barSplit(rows: Seq[String]) = {
      rows.filterNot(_.isEmpty).foldLeft((Seq[Seq[String]](), Seq[Seq[String]]()))((total, s) => {
        val (l, r) = s.span(!_.equals('|'))
        (total._1 :+ l.map(_.toString), total._2 :+ r.headOption.map(x => r.tail.map(_.toString)).getOrElse(Seq()))
      })
    }
    val (headers,below) = s.stripMargin('^').split("\n").span(!_.contains("-"))
    val (rowColumnHeaders, columnHeaders) = barSplit(headers)
    val (rowHeaders, main) = below.headOption.map(x => below.tail).map(rows => barSplit(rows)).getOrElse((Seq(), Seq()))
    ReportContent(rowHeaders, columnHeaders, main, rowColumnHeaders) //.map(_.map(_.toString))
  }
}
