package org.suecarter.tablediff

import org.scalatest.FunSuite
import org.suecarter.tablediff.TableDiff._
import scala.io.Source
import java.io.File
import HTMLTableDiff._

class HTMLTableDiffTests extends FunSuite {
  val tmpDir = new File(System.getProperty("java.io.tmpdir") + File.separatorChar + "TableDiffTestsHTMLFiles")
  tmpDir.mkdirs()
  test("diff html reports") {
    val cases = List(
      ("recA2.json", "recB2.json")
    )

    cases.foreach {
      case (l, r) =>
        val sourceLeft = Source.fromURL(getClass.getResource("/" + l), "latin1")
        val leftReport = fromJsonTable(sourceLeft.getLines().mkString(" "))
        val bounceReport = fromJsonTable(toJsonTable(leftReport))
        assert(bounceReport === leftReport)
        val bounce2Report = fromJsonTable(toHTMLString(leftReport, "cat"))
        assert(bounce2Report === leftReport)
        val sourceRight = Source.fromURL(getClass.getResource("/" + r), "latin1")
        val rightReport = fromJsonTable(sourceRight.getLines().mkString(" "))
        val diffReport: ReportContent[ValueDiff[String], ValueDiff[String], ValueDiff[String]] = produceReportDiff(leftReport, rightReport)
    //    println(StringTableDiff.diffReportToString(diffReport))
        assert(!leftReport.isEmpty)
        assert(!rightReport.isEmpty)
        assert(!diffReport.isEmpty)
//        println(StringTableDiff.diffReportToString(TableDiff.onlyTheDiffs(diffReport)))
      HTMLTableDiff.writeHTMLDiffAndContext(l, tmpDir, diffReport)
    }
  }
  test("escape strings") {
    import TableDiffTestCases._
    import scala.{List => L}
    val report = ReportContent(
      L(L("a", "\"<test>&b</test>\""), L("b", "d")),
      L(L("Col1", "Col2", "Col3"),
        L("one", "two", "three")),
      L(L(7, 5, 2),
        L(33, 1, 34)),
      L(L("AHead", "BHead"), L("","")))
    val bounceReport = fromJsonTable(toJsonTable(report))
    assert(bounceReport.toString === report.toString)
    HTMLTableDiff.writeHTMLFile("sue", tmpDir, report)
  }
  test("render simple tables") {
    import StringTableDiffTests._

    StringTableDiffTests.cases.zipWithIndex.foreach {
      case ((l, r), i) =>
        HTMLTableDiff.writeHTMLFile("sue" + i, tmpDir, l)
        // check that a bounce loses no data
        def toString = (x: Any) => x.toString
        val startReport = l.mapAllCells(toString)
        val a = toJsonTable(startReport)
        val bounceReport = fromJsonTable(toJsonTable(startReport))
        val diffReport = produceReportDiff(startReport, bounceReport)
//        println("Diff is \n" + StringTableDiff.diffReportToString(diffReport))
        val onlyDiffs = onlyTheDiffs(diffReport)
//        println("OnlyDiffs is \n" + StringTableDiff.diffReportToString(onlyDiffs))
        assert(onlyDiffs.isEmpty,
          "Before\n" + StringTableDiff.diffReportToString(startReport) +
          "After\n" + StringTableDiff.diffReportToString(bounceReport))
    }

  }
}
