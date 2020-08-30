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
  def diffReportToString(report: ReportContent[_, _, _]) =
    if (report.isEmpty) {
      "Empty report"
    } else {
      // take a report section and turn it into a ReportSection with all entries being ValueDiffs
      // the flatten fns take ValueDiff ReportSections
      def valueDiffise[T](reportSection: ReportSection[T]): ReportSection[ValueDiff[Any]] =
        reportSection.map(_.map { x =>
          val diffValue: ValueDiff[Any] = x match {
            case diff: ValueDiff[Any @unchecked] => diff //unchecked as we don't care about the type in ValueDiff
            case value                           => Right(Some(value))
          }
          diffValue
        })

      def charRepeat(c: Char, count: Int) = new String((0 until count).map(x => c).toArray)
      val diffReport: ReportContent[ValueDiff[Any], ValueDiff[Any], ValueDiff[Any]] =
        report.copy(
          rowHeaders = valueDiffise(report.rowHeaders),
          columnHeaders = valueDiffise(report.columnHeaders),
          mainData = valueDiffise(report.mainData),
          rowColumnHeaders = valueDiffise(report.rowColumnHeaders)
        )

      // If the cell contains a multi line string, then extend the report rows
      def extendMultiLine(rows: ReportSection[String]): ReportSection[String] =
        rows.flatMap { row =>
          val splitRows = row.map(_.split("\n").toSeq)
          val rowLength = splitRows.map(_.size).max
          TableDiff.pivotHeaders[String](splitRows.map(row => row ++ (row.size until rowLength).map(x => "")))
        }

      val columnHeaders = extendMultiLine(flattenColumnHeaders(diffReport).map(h => h.map(valueDiffRenderer(_))))
      val rows = extendMultiLine(flattenTableRows(diffReport).map(h => h.map(valueDiffRenderer(_))))

      def maxStringWidth(headers: ReportSection[String]): Seq[Int] = {
        @tailrec
        def inner(accumulator: Seq[Int], headers: ReportSection[String]): Seq[Int] =
          headers.filter(_.nonEmpty) match {
            case Nil => accumulator
            case hs  => inner(accumulator :+ hs.map(_.head.size).max, hs.map(_.tail))
          }
        inner(Seq(), headers)
      }

      val columnWidths = maxStringWidth(columnHeaders ++ rows)
      def horizontalLine(char: Char = '-') = {
        val (l, r) = columnWidths.splitAt(report.rowWidth)
        (if (l.isEmpty) "" else l.map(i => charRepeat(char, i)).mkString("+", "-", if (r.isEmpty) "+" else "")) +
          (if (r.isEmpty) "" else r.map(i => charRepeat(char, i)).mkString("+", "-", "+"))
      }
      def resizeRowCells(row: Seq[String]) =
        row.zipWithIndex.map {
          case (cell, i) => cell + charRepeat(' ', columnWidths(i) - cell.size)
        }.mkString("|", "|", "|") + {
          val sizeMissing = columnWidths.size - row.size
          if (sizeMissing > 0)
            (row.size until columnWidths.size).map(i => charRepeat(' ', columnWidths(i))).mkString("", "|", "|")
          else ""
        }

      // produce output string. Resizing cells as we go, to square off the grid
      (if (report.columnDepth > 0)
         horizontalLine('-') + "\n" + columnHeaders.map(resizeRowCells).mkString("", "\n", "\n")
       else "") +
        horizontalLine('-') + "\n" +
        (if (rows.nonEmpty) {
           rows.map(resizeRowCells).mkString("", "\n", "\n") +
             horizontalLine('-') + "\n"
         } else "")
    }

  // generate in-place style diffs
  def valueDiffRenderer[T](
      value: ValueDiff[T],
      valueRenderer: (T) => String = (x: T) => x.toString,
      missingLeft: String = "[-",
      addedLeft: String = "{+",
      sameLeft: String = "",
      missingRight: String = "-]",
      addedRight: String = "+}",
      sameRight: String = "",
      chunkSize: Option[Int] = None,
      complexityThreshold: Int = 4
  ) =
    value.fold(
      l => {
        val leftString = l.left.map(x => if (isEmpty(x)) "" else valueRenderer(x)).getOrElse("")
        val rightString = l.right.map(x => if (isEmpty(x)) "" else valueRenderer(x)).getOrElse("")

        val diffs = chunkSize
          .map(c => zipLongestCommonSubsequence(leftString, rightString, c))
          .getOrElse(zipLongestCommonSubsequence(leftString, rightString))
        val onlyNumericDiffs = {
          case class NumberState(
              digitYet: Boolean = false,
              digitsAndLetters: Boolean = false,
              decimalPointYet: Boolean = false,
              onlyNumerics: Boolean = false,
              numericMinusYet: Boolean = false
          )
          val diffElements = diffs.span(!_.hasANone)._2.reverse.span(!_.hasANone)._2
          diffElements
            .foldLeft(NumberState()) {
              (state, diffLoc) =>
                val s = diffLoc.value match {
                  case _ if state.digitsAndLetters => state // never be true once digitsAndLetters is true
                  case m if m == '-' && !state.digitYet =>
                    state.copy(numericMinusYet = true, digitYet = true, onlyNumerics = true)
                  case p if p == '.' && state.decimalPointYet => state.copy(onlyNumerics = false)
                  case p if p == '.'                          => state.copy(decimalPointYet = true)
                  case d if Character.isDigit(d)              => state.copy(digitYet = true, onlyNumerics = true)
                  case _                                      => state.copy(digitsAndLetters = true, onlyNumerics = false)
                }
                s
            }
            .onlyNumerics
        }
        // little helper class to manage the complexity of the diff, default being no complexity
        case class DiffComplex(value: String = "", complex: Int = 0) {
          def prependValue(pre: Char) = this.copy(value = pre + value)
        }
        def decorate(xOpt: Option[DiffLocation[Char]], yOpt: Option[DiffLocation[Char]]) = {
          val typeChange = xOpt.map(x => x.locationType) != yOpt.map(y => y.locationType)
          if (typeChange) {
            val xDiff = xOpt
              .map(_.locationType match {
                case OnlyLeft  => DiffComplex(missingRight, 1)
                case OnlyRight => DiffComplex(addedRight, 1)
                case InBoth    => DiffComplex(sameRight, 0)
              })
              .getOrElse(DiffComplex())
            val yDiff = yOpt
              .map(_.locationType match {
                case OnlyLeft  => DiffComplex(missingLeft, 0) // for every open there's a close
                case OnlyRight => DiffComplex(addedLeft, 0) // so no need to add complexity
                case InBoth    => DiffComplex(sameLeft, 0)
              })
              .getOrElse(DiffComplex())
            DiffComplex(xDiff.value + yDiff.value, xDiff.complex + yDiff.complex)
          } else DiffComplex()
        }
        val inPlaceDiff =
          diffs.headOption.map(head => decorate(None, Some(head))).getOrElse(DiffComplex()) +:
            diffs
              .sliding(2)
              .map {
                case x :: y :: Nil => decorate(Some(x), Some(y)).prependValue(x.value)
                case _             => DiffComplex() // Do nothing here as it's caught by tailOption and headOption bits
              }
              .toList :+
            diffs.lastOption.map(tail => decorate(Some(tail), None).prependValue(tail.value)).getOrElse(DiffComplex())
        // somewhat arbitrary, but if the inplace diff is longer than a simple diff, just return the simple one
        if (onlyNumericDiffs || inPlaceDiff.map(_.complex).sum >= complexityThreshold)
          l.left.map(x => if (isEmpty(x)) "" else missingLeft + valueRenderer(x) + missingRight).getOrElse("") +
            l.right.map(x => if (isEmpty(x)) "" else addedLeft + valueRenderer(x) + addedRight).getOrElse("")
        else
          inPlaceDiff.map(_.value).mkString
      },
      r => r.map(sameLeft + _.toString + sameRight).getOrElse("")
    )

}
