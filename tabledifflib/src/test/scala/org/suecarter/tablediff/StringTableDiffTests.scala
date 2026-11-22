package org.suecarter.tablediff

import org.scalatest.funsuite.AnyFunSuite
import TableDiff._

class StringTableDiffTests extends AnyFunSuite {
  test("reports to string") {
    StringTableDiffTests.cases.foreach {
      case (l, r) =>
        //println(StringTableDiff.diffReportToString(l))
        assert(StringTableDiff.diffReportToString(l) === r.stripMargin('^'))
        assert(
          onlyTheDiffs(produceReportDiff(l, l)).isEmpty,
          "Report " + l + " does not match itself on a reflective diff"
        )
    }
  }
}

object StringTableDiffTests {
  val topLeft = List(List("", ""), List("a", "b"))
  val multiLineTopLeft = List(List("", ""), List("a", """b
                                                        |z""".stripMargin))
  val topRight = List(List("d", ""), List("f", "g"))
  val multiLineTopRight = List(List("""d
      |z""".stripMargin, ""), List("f", "g"))
  val bottomLeft = List(List("M", "N"), List("", "O"))
  val multiLineBottomLeft = List(List("M", """N
      |Z""".stripMargin), List("", "O"))
  val bottomRight = List(List(1, 2), List(3))
  val emptySquare = Seq(Seq("", ""), Seq("", ""))
  val cases = List(
    (TableDiffTestCases.longReport, """^+----+------+
                                       ^|Row1|11|223|
                                       ^|Row2|12|223|
                                       ^|Row3|13|223|
                                       ^|Row4|14|223|
                                       ^|Row5|15|223|
                                       ^|Row6|16|223|
                                       ^+----+------+
                                      ^"""),
    (ReportContent(bottomLeft, topRight, bottomRight, emptySquare), """^+---+---+
                                       ^| | |d| |
                                       ^| | |f|g|
                                       ^+---+---+
                                       ^|M|N|1|2|
                                       ^| |O|3| |
                                       ^+---+---+
                                      ^"""),
    (ReportContent(multiLineBottomLeft, topRight, bottomRight, multiLineTopLeft), """^+---+---+
                                       ^| | |d| |
                                       ^|a|b|f|g|
                                       ^| |z| | |
                                       ^+---+---+
                                       ^|M|N|1|2|
                                       ^| |Z| | |
                                       ^| |O|3| |
                                       ^+---+---+
                                      ^"""),
    (ReportContent(multiLineBottomLeft, multiLineTopRight, bottomRight, multiLineTopLeft), """^+---+---+
                                       ^| | |d| |
                                       ^| | |z| |
                                       ^|a|b|f|g|
                                       ^| |z| | |
                                       ^+---+---+
                                       ^|M|N|1|2|
                                       ^| |Z| | |
                                       ^| |O|3| |
                                       ^+---+---+
                                      ^"""),
    (ReportContent(Seq(), topRight, bottomRight), """^+---+
                                         ^|d| |
                                         ^|f|g|
                                         ^+---+
                                         ^|1|2|
                                         ^|3| |
                                         ^+---+
                                        ^"""),
    (ReportContent(bottomLeft, Seq(), bottomRight), """^+---+---+
                                         ^|M|N|1|2|
                                         ^| |O|3| |
                                         ^+---+---+
                                        ^"""),
    (ReportContent(bottomLeft, topRight.map(_ ++ "i"), Seq(), emptySquare), """^+---+-----+
                                         ^| | |d| |i|
                                         ^| | |f|g|i|
                                         ^+---+-----+
                                         ^|M|N| | | |
                                         ^| |O| | | |
                                         ^+---+-----+
                                        ^"""),
    (ReportContent(Seq(), Seq(), bottomRight), """^+---+
                                         ^|1|2|
                                         ^|3| |
                                         ^+---+
                                        ^"""),
    (ReportContent(bottomLeft, Seq(), Seq()), """^+---+
                                         ^|M|N|
                                         ^| |O|
                                         ^+---+
                                        ^"""),
    (ReportContent(Seq(), topRight, Seq()), """^+---+
                                         ^|d| |
                                         ^|f|g|
                                         ^+---+
                                        ^"""),
    (ReportContent(bottomLeft, topRight, bottomRight, topLeft), """^+---+---+
                                           ^| | |d| |
                                           ^|a|b|f|g|
                                           ^+---+---+
                                           ^|M|N|1|2|
                                           ^| |O|3| |
                                           ^+---+---+
                                          ^"""),
    (ReportContent(emptySquare, topRight, bottomRight, topLeft), """^+---+---+
                                             ^| | |d| |
                                             ^|a|b|f|g|
                                             ^+---+---+
                                             ^| | |1|2|
                                             ^| | |3| |
                                             ^+---+---+
                                            ^"""),
    (ReportContent(bottomLeft, Seq(), bottomRight, topLeft), """^+---+---+
                                             ^| | | | |
                                             ^|a|b| | |
                                             ^+---+---+
                                             ^|M|N|1|2|
                                             ^| |O|3| |
                                             ^+---+---+
                                            ^"""),
    (ReportContent(bottomLeft, topRight.map(_ ++ "i"), Seq(), topLeft), """^+---+-----+
                                             ^| | |d| |i|
                                             ^|a|b|f|g|i|
                                             ^+---+-----+
                                             ^|M|N| | | |
                                             ^| |O| | | |
                                             ^+---+-----+
                                            ^"""),
    (ReportContent(emptySquare, Seq(), bottomRight, topLeft), """^+---+---+
                                             ^| | | | |
                                             ^|a|b| | |
                                             ^+---+---+
                                             ^| | |1|2|
                                             ^| | |3| |
                                             ^+---+---+
                                            ^"""),
    (ReportContent(bottomLeft, Seq(), Seq(), topLeft), """^+---+
                                             ^| | |
                                             ^|a|b|
                                             ^+---+
                                             ^|M|N|
                                             ^| |O|
                                             ^+---+
                                            ^"""),
    (ReportContent(Seq(), topRight, Seq(), topLeft), """^+---+---+
                                             ^| | |d| |
                                             ^|a|b|f|g|
                                             ^+---+---+
                                            ^""")
  )
}
