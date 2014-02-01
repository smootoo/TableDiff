package org.suecarter.tablediff

import scala.annotation.tailrec

object StringTableDiff {

  import TableDiff._
  import ReportContent._

  /**
   *
   * @param report
   * @return String representation of the Report
   */
  def diffReportToString(report: ReportContent[_, _, _]) = {
    if (report.isEmpty) {
      "Empty report"
    } else {
      // take a report section and turn it into a ReportSection with all entries being ValueDiffs
      // the flatten fns take ValueDiff ReportSections
      def valueDiffise[T](reportSection: ReportSection[T]): ReportSection[ValueDiff[Any]] = {
        reportSection.map(_.map {
          x => {
            val diffValue: ValueDiff[Any] = x match {
              case diff: ValueDiff[Any] => diff
              case value => Right(Some(value))
            }
            diffValue
          }
        })
      }
      // generate in-place style diffs
      def valueDiffRenderer[T](value: ValueDiff[T]) = {
        value.fold(l =>
          l.left.map(x => if (x.toString == "") "" else "[-" + x + "-]").getOrElse("") +
            l.right.map(x => if (x.toString == "") "" else "{+" + x + "+}").getOrElse(""),
          r => r.map(_.toString).getOrElse(""))
      }
      def charRepeat(c: Char, count: Int) = new String((0 until count).map(x => c).toArray)
      val diffReport: ReportContent[ValueDiff[Any], ValueDiff[Any], ValueDiff[Any]] =
        report.copy(
          rowHeaders = valueDiffise(report.rowHeaders),
          columnHeaders = valueDiffise(report.columnHeaders),
          mainData = valueDiffise(report.mainData),
          rowColumnHeaders = valueDiffise(report.rowColumnHeaders))
      val columnHeaders = flattenColumnHeaders(diffReport).map(h => h.map(valueDiffRenderer))
      val rows = flattenTableRows(diffReport).map(h => h.map(valueDiffRenderer))
      val tableRows = columnHeaders ++ rows

      def maxStringWidth(headers: ReportSection[String]): Seq[Int] = {
        @tailrec
        def inner(accumulator: Seq[Int], headers: ReportSection[String]): Seq[Int] = {
          headers.filter(!_.isEmpty) match {
            case Nil => accumulator
            case hs => inner(accumulator :+ hs.map(_.head.size).max, hs.map(_.tail))
          }
        }
        inner(Seq(), headers)
      }

      val columnWidths = maxStringWidth(tableRows)
      def horizontalLine(char: Char = '-') = {
        val (l, r) = columnWidths.splitAt(report.rowWidth)
        (if (l.isEmpty) "" else l.map(i => charRepeat(char, i)).mkString("+", "-", if (r.isEmpty) "+" else "")) +
          (if (r.isEmpty) "" else r.map(i => charRepeat(char, i)).mkString("+", "-", "+"))
      }
      def resizeRowCells(row: Seq[String]) = row.zipWithIndex.map {
        case (cell, i) => cell + charRepeat(' ', columnWidths(i) - cell.size)
      } .mkString("|", "|", "|") + {
        val sizeMissing = columnWidths.size - row.size
        if (sizeMissing > 0)
          (row.size until columnWidths.size).map(i => charRepeat(' ',columnWidths(i))).mkString("", "|", "|")
        else ""}

      // produce output string. Resizing cells as we go, to square off the grid
      (if (report.columnDepth > 0)
        horizontalLine('-') + "\n" + columnHeaders.map(resizeRowCells).mkString("", "\n", "\n")
      else "") +
        horizontalLine('-') + "\n" +
        (if (!rows.isEmpty) {
          rows.map(resizeRowCells).mkString("", "\n", "\n") +
            horizontalLine('-') + "\n"
        } else "")
    }
  }
}

