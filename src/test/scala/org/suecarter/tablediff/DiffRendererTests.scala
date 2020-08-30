package org.suecarter.tablediff

import org.scalatest.funsuite.AnyFunSuite

class DiffRendererTests extends AnyFunSuite {
  test("rendering of diff values") {
    val cases = List(
      ("146", "142", "[-146-]{+142+}"),
      ("146*", "146", "146[-*-]"),
      ("146.3", "146.2", "[-146.3-]{+146.2+}"),
      ("1463", "146.3", "146{+.+}3"),
      ("42", "(1)", "[-42-]{+(1)+}"),
      ("(240)", "(194)", "[-(240)-]{+(194)+}"),
      ("912", "870", "[-912-]{+870+}"),
      ("192", "(9)", "[-192-]{+(9)+}"),
      ("(1,362)", "(1,605)", "[-(1,362)-]{+(1,605)+}"),
      ("cat", "cab", "ca[-t-]{+b+}"),
      ("catd", "cabdf", "ca[-t-]{+b+}d{+f+}"),
      ("catd", "cabdfg", "ca[-t-]{+b+}d{+fg+}"),
      ("catde", "cabdfg", "[-catde-]{+cabdfg+}"),
      ("catde", "cat.de", "cat{+.+}de"),
      ("1246", "12x6", "12[-4-]{+x+}6"),
      ("124.6", "124..6", "124.{+.+}6"),
      ("124.6", "124...6", "124.{+..+}6"),
      ("-124.6", "124...6", "[---]124{+..+}.6"),
      (None, "", ""),
      (None, None, ""),
      ("", None, "")
    )
    import TableDiffTestCases._
    val exampleSections = cases.map {
      case (l, r, v) =>
        //println(StringTableDiff.diffReportToString(l))
        val diffValue = Left(EitherSide(Some(l), Some(r)))
        val result = StringTableDiff.valueDiffRenderer(diffValue)
        assert(
          result === v,
          "In-place does not match. Left " + l + " Right " + r + " Expected " + v + " but got " + result
        )
        (Seq(D(l), D(r)), Seq(D(HTMLTableDiff.valueDiffRenderer(diffValue))))
    }
    HTMLTableDiff.writeHTMLFile(
      "RendererExamples",
      HTMLTableDiffTests.tmpDir,
      ReportContent(
        exampleSections.map(_._1),
        Seq(Seq(D("RenderedDiff"))),
        exampleSections.map(_._2),
        Seq(Seq(D("Left"), D("Right")))
      )
    )
  }
}
