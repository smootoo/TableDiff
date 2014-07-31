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

      def charRepeat(c: Char, count: Int) = new String((0 until count).map(x => c).toArray)
      val diffReport: ReportContent[ValueDiff[Any], ValueDiff[Any], ValueDiff[Any]] =
        report.copy(
          rowHeaders = valueDiffise(report.rowHeaders),
          columnHeaders = valueDiffise(report.columnHeaders),
          mainData = valueDiffise(report.mainData),
          rowColumnHeaders = valueDiffise(report.rowColumnHeaders))
      val columnHeaders = flattenColumnHeaders(diffReport).map(h => h.map(valueDiffRenderer(_)))
      val rows = flattenTableRows(diffReport).map(h => h.map(valueDiffRenderer(_)))
      val tableRows = columnHeaders ++ rows

      def maxStringWidth(headers: ReportSection[String]): Seq[Int] = {
        @tailrec
        def inner(accumulator: Seq[Int], headers: ReportSection[String]): Seq[Int] = {
          headers.filter(_.nonEmpty) match {
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

  // generate in-place style diffs
  def valueDiffRenderer[T](value: ValueDiff[T],
                           valueRenderer: (T) => String = (x: T) => x.toString,
                           missingLeft: String = "[-",
                           addedLeft: String = "{+",
                           sameLeft:String = "",
                           missingRight: String = "-]",
                           addedRight: String = "+}",
                           sameRight:String = "") = {
    value.fold(l => {
      val leftString = l.left.map(valueRenderer).getOrElse("")
      val rightString = l.right.map(valueRenderer).getOrElse("")

      val diffs = zipLongestCommonSubsequence(leftString, rightString)
      // little helper class to manage the complexity of the diff, default being no complexity
      case class DiffComplex(value: String = "", complex: Int = 0) {
        def prependValue(pre: Char) = this.copy(value = pre + value)
      }
      def decorate(xOpt: Option[DiffLocation[Char]], yOpt: Option[DiffLocation[Char]]) = {
        val typeChange = xOpt.map(x => x.locationType) != yOpt.map(y => y.locationType)
        if (typeChange) {
          val xDiff = xOpt match {
            case Some(x) if x.locationType == OnlyLeft => DiffComplex(missingRight, 1)
            case Some(x) if x.locationType == OnlyRight => DiffComplex(addedRight, 1)
            case Some(x) if x.locationType == InBoth => DiffComplex(sameRight, 0)
            case _ => DiffComplex()
          }
          val yDiff = yOpt match {
            case Some(y) if y.locationType == OnlyLeft => DiffComplex(missingLeft, 0) // for every open there's a close
            case Some(y) if y.locationType == OnlyRight => DiffComplex(addedLeft, 0)  // so no need to add complexity
            case Some(x) if x.locationType == InBoth => DiffComplex(sameLeft, 0)
            case _ => DiffComplex()
          }
          DiffComplex(xDiff.value + yDiff.value, xDiff.complex + yDiff.complex)
        } else DiffComplex()
      }
      val inPlaceDiff =
        diffs.headOption.map(head => decorate(None, Some(head))).getOrElse(DiffComplex()) +:
          diffs.sliding(2).map {
            case x :: y :: Nil => decorate(Some(x), Some(y)).prependValue(x.value)
            case _ => DiffComplex() // Do nothing here as it's caught by tailOption and headOption bits
          }.toList :+
          diffs.lastOption.map(tail => decorate(Some(tail), None).prependValue(tail.value)).getOrElse(DiffComplex())
      // somewhat arbitrary, but if the inplace diff is longer than a simple diff, just return the simple one
      if (inPlaceDiff.map(_.complex).sum >= 4)
        l.left.map(x => missingLeft + valueRenderer(x) + missingRight).getOrElse("") +
          l.right.map(x => addedLeft + valueRenderer(x) + addedRight).getOrElse("")
      else
        inPlaceDiff.map(_.value).mkString
    }, r => sameLeft + r.map(_.toString).getOrElse("") + sameRight)
  }


}

