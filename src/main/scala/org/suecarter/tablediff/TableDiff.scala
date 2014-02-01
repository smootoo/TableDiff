package org.suecarter.tablediff

import scala.annotation.tailrec
import ReportContent._

/**
 * Value differs on side to the other. None means the value is missing on that side
 * @param left
 * @param right
 * @tparam T
 */
case class EitherSide[T](left: Option[T], right: Option[T])

case class DiffLocation[T](value: T, iLeft: Option[Int], iRight: Option[Int]) {
  def hasANone = iLeft == None || iRight == None
}

object TableDiff {
  /**
   * Represents a value that could be a diff. Left is a diff. Right is not a diff, but is an option
   * @tparam T
   */
  type ValueDiff[T] = Either[EitherSide[T], Option[T]]

  private def pivotHeaders[T](reportSection: ReportSection[T]): ReportSection[T] = {
    @tailrec
    def inner(accumulator: ReportSection[T], r: ReportSection[T]): ReportSection[T] =
    r.filter(!_.isEmpty) match {
      case Nil => accumulator
      case hs: ReportSection[T] => inner(accumulator :+ hs.map(_.head), hs.map(_.tail))
    }
    inner(emptyRow, reportSection)
  }

  // Take a diff of a sequence and turn it into a Seq of individual diffs
  private def splitSeqDiff[T](seqDiff: DiffLocation[ValueDiff[ReportRow[T]]]): ReportRow[ValueDiff[T]] = {
    seqDiff.value.fold(
      l => {
        val leftSeq = l.left.getOrElse(emptyRow).map(Option(_))
        val rightSeq = l.right.getOrElse(emptyRow).map(Option(_))
        for((left, right) <- leftSeq.zipAll(rightSeq, None, None)) yield
        if (left == right)
          Right(left)
        else
          Left(EitherSide(left, right))
      },
      r => r.map(x => x.map(y => Right(Some(y)))).getOrElse(emptyRow))
  }

  private def emptyDiffCells(i: Int) = (0 until i).map(x => Right(None))

  // fill in space at top left of table column header if needed
  protected[tablediff] def flattenColumnHeaders[C](report: ReportContent[_, ValueDiff[C], _]) = {
    val paddedRowColumnHeaders = (report.rowColumnHeaders.size to report.columnDepth-1).map(x => Seq()) ++ report.rowColumnHeaders
    paddedRowColumnHeaders.zipAll(report.columnHeaders, Seq(), Seq()).map {
      case (rch, ch) => rch ++ emptyDiffCells(report.rowWidth - rch.size) ++ ch ++ emptyDiffCells(report.columnCount - ch.size)
    }
  }

  // join up row headers and main table section
  protected[tablediff] def flattenTableRows[T, R <: T, M <: T](report: ReportContent[ValueDiff[R], _, ValueDiff[M]])
  : ReportSection[ValueDiff[T]] =
  {
    report.rowHeaders.zipAll(report.mainData, Seq(), Seq()).map {
      case (r, m) => r ++ emptyDiffCells(report.rowWidth - r.size) ++ m ++ emptyDiffCells(report.mainDataColumnCount - m.size)
      // asInstanceOf here as compiler can't work it out, but is safe R,M <: T
    }.map(_.asInstanceOf[ReportRow[ValueDiff[T]]])
  }

  /**
   * flatten row, column, main table structure to a single section
   * @param report
   * @tparam T return ReportSection type T is the type that is a supertype of R,C,M
   * @tparam R row header ValueDiff type
   * @tparam C column header ValueDiff type
   * @tparam M main data header ValueDiff type
   * @return
   */
  def flattenDiffReport[T, R <: T, C <: T, M <: T](report: ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]]): ReportSection[ValueDiff[T]] = {
    val columnHeaders = flattenColumnHeaders(report)
    val rows = flattenTableRows(report)
    // asInstanceOfs are needed as the compiler can't work it out, but it is safe as R,C,M <: T
    columnHeaders.map(_.asInstanceOf[ReportRow[ValueDiff[T]]]) ++ rows.map(_.asInstanceOf[ReportRow[ValueDiff[T]]])
  }

  /**
   *
   * @param diffReport
   * @tparam R Row header type
   * @tparam C Column header type
   * @tparam M Main data type
   * @return the input diff report filtered to just rows and columns containing diffs
   */
  def onlyTheDiffs[R, C, M](diffReport: ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]]) = {
    val report = if (diffReport.fillForwardBlankHeaders) fillForwardReportHeaders(diffReport) else diffReport
    val (filteredRows, mainData) =
      report.rowHeaders.zipAll(report.mainData, Seq(Right(None)), Seq(Right(None))).filter {
        case (r, m) => !(r.filter(_.isLeft).isEmpty && m.filter(_.isLeft).isEmpty)
      }.unzip
    val (cols, main) =
      pivotHeaders(report.columnHeaders).zipAll(pivotHeaders(mainData), Seq(Right(None)), Seq(Right(None))).filter {
        case (r, m) => !(r.filter(_.isLeft).isEmpty && m.filter(_.isLeft).isEmpty)
      }.unzip
    val filteredColumns = pivotHeaders(cols)
    val filteredMain = pivotHeaders(main)
    val filteredReport = ReportContent(filteredRows, filteredColumns, filteredMain,
      if ((!filteredRows.isEmpty) || !report.rowColumnHeaders.filter(!_.filter(_.isLeft).isEmpty).isEmpty)
        report.rowColumnHeaders else emptySection)
    if (diffReport.fillForwardBlankHeaders) removeDuplicateReportHeaders(filteredReport) else filteredReport
  }

  /**
   * Produce a report that is the diff of the left and right report
   * @param leftReport
   * @param rightReport
   * @tparam R Row header type
   * @tparam C Column header type
   * @tparam M Main data type
   * @return
   */
  def produceReportDiff[R, C, M](leftReport: ReportContent[R, C, M],
                                 rightReport: ReportContent[R, C, M])
    : ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]] = {
    // get the value from the main data corresponsing to this row and column indexes
    def mainValue(rowIndex: Option[Int], colIndex: Option[Int], report: ReportContent[R, C, M]): Option[M] =
      rowIndex.flatMap(r => colIndex.flatMap(c => if (r < report.mainData.size && c < report.mainData(r).size) {
        val a = report.mainData(r)(c)
        Some(a)
      } else None))
    def headerValue[T](h: DiffLocation[T]): ValueDiff[T] =
      if (h.hasANone)
        Left(EitherSide(h.iLeft.map(x => h.value), h.iRight.map(a => h.value)))
      else
        Right(Some(h.value))
    def mainValueDiff(row: DiffLocation[_], col: DiffLocation[_]): ValueDiff[M] = {
      val left = mainValue(row.iLeft, col.iLeft, leftReport)
      val right = mainValue(row.iRight, col.iRight, rightReport)
      if (left != right) Left(EitherSide(left, right)) else Right(left)
    }
    // this logic tries to collapse headers diff back to a single ValueDiff if the mainData matches
    // (i.e. only the headerValue is a diff). Will only search "one" row down for match
    def collapseHeaders[T](headerRows: ReportRow[DiffLocation[T]], leftMainData: ReportSection[M], rightMainData: ReportSection[M])
    : ReportRow[DiffLocation[ValueDiff[T]]] = {
      @tailrec
      def inner[S](accumulator: ReportRow[DiffLocation[ValueDiff[S]]], headerRows: ReportRow[DiffLocation[S]], leftMainData: ReportSection[M], rightMainData: ReportSection[M])
      : ReportRow[DiffLocation[ValueDiff[S]]] = {
        if (headerRows.isEmpty)
          accumulator
        else {
          val (head, tailSection): (DiffLocation[ValueDiff[S]], ReportRow[DiffLocation[S]]) = headerRows match {
            case Seq(dlLeft@DiffLocation(leftValue, Some(leftI), None), tail@_*) => { // find row with just a left value
              val matchingRightValue: Option[(DiffLocation[TableDiff.ValueDiff[S]], ReportRow[DiffLocation[S]])] = tail.collectFirst {
                case dlRight@DiffLocation(rightValue, None, Some(rightI)) => { // try to match to row with just a right value
                  val leftI = dlLeft.iLeft.getOrElse(0)
                  val rightI = dlRight.iRight.getOrElse(0)
                  if ((leftMainData.size == 0 && rightMainData.size == 0) // either main data is empty
                    || ((leftMainData.size >= leftI + 1) &&               // or indexes point to same elements, 1 row away
                        (rightMainData.size >= rightI + 1) &&
                        leftMainData(leftI) == rightMainData(rightI)))
                    (DiffLocation(Left(EitherSide(Some(dlLeft.value), Some(dlRight.value))), Some(leftI), Some(rightI)),
                      tail.filterNot(_ == dlRight))
                  else
                    (DiffLocation(headerValue(dlLeft), Some(leftI), None), tail)
                }
              }
              matchingRightValue.getOrElse((DiffLocation(headerValue(dlLeft), Some(leftI), None), tail))
            }
            case Seq(dl@DiffLocation(_, leftI, rightI), tail@_*) =>
              (DiffLocation(headerValue(dl), leftI, rightI), tail)
          }
          inner(accumulator :+ head, tailSection, leftMainData, rightMainData)
        }
      }
      inner(emptyRow, headerRows, leftMainData, rightMainData)
    }
    // used to fill in headers such that they line up with the mainData section
    def extraHeaders[S](howManyLeft: Int, startIndexLeft: Int, howManyRight: Int, startIndexRight: Int): ReportRow[DiffLocation[ValueDiff[S]]] =
      (0 until howManyLeft).map(Option(_)).zipAll((0 until howManyRight).map(Option(_)), None, None).map {
        case (l, r) =>
          DiffLocation[ValueDiff[S]](Right(None), l.map(_ + startIndexLeft), r.map(_ + startIndexRight))
      }
    def zipLCSColumnSection(leftSection: ReportSection[C],
                            rightSection: ReportSection[C]): ReportRow[DiffLocation[ReportRow[C]]] = {
      zipLongestCommonSubsequence(
        if (leftReport.fillForwardBlankHeaders)
          fillSectionHeaders(pivotHeaders(leftSection))
        else
          pivotHeaders(leftSection),
        if (rightReport.fillForwardBlankHeaders)
          fillSectionHeaders(pivotHeaders(rightSection))
        else
          pivotHeaders(rightSection))
    }
    val resultCols = zipLCSColumnSection(leftReport.columnHeaders, rightReport.columnHeaders)
    val resultRowColHeaders = zipLCSColumnSection(leftReport.rowColumnHeaders, rightReport.rowColumnHeaders)
    def flattenRowHeaderSection(report: ReportContent[R, _, _]) ={
      if (report.rowSectionWidth > 0 ) report.rowHeaders else (0 until report.mainDataRows).map(x => Seq[R]())
    }
    val leftRowHeaders = flattenRowHeaderSection(leftReport)
    val rightRowHeaders = flattenRowHeaderSection(rightReport)
    val resultRows = zipLongestCommonSubsequence(
      if (leftReport.fillForwardBlankHeaders) fillSectionHeaders(leftRowHeaders) else leftRowHeaders,
      if (rightReport.fillForwardBlankHeaders) fillSectionHeaders(rightRowHeaders) else rightRowHeaders)
    val rows: ReportRow[DiffLocation[ValueDiff[ReportRow[R]]]] =
      collapseHeaders(resultRows, leftReport.mainData, rightReport.mainData) ++
        extraHeaders(leftReport.mainDataRows - leftRowHeaders.size, leftRowHeaders.size,
                rightReport.mainDataRows - rightRowHeaders.size, rightRowHeaders.size)
    val cols: ReportRow[DiffLocation[ValueDiff[ReportRow[C]]]] =
      collapseHeaders(resultCols, pivotHeaders(leftReport.mainData), pivotHeaders(rightReport.mainData)) ++
      extraHeaders(leftReport.mainDataColumnCount - leftReport.columnCount, leftReport.columnCount,
        rightReport.mainDataColumnCount - rightReport.columnCount, rightReport.columnCount)
    val rowsContent: ReportSection[ValueDiff[R]] = rows.map(splitSeqDiff)
    val rowColumnHeadersContent: ReportSection[ValueDiff[C]] = collapseHeaders(resultRowColHeaders, Seq(), Seq()).map(splitSeqDiff)
    val colsContent: ReportSection[ValueDiff[C]] = cols.map(splitSeqDiff)
    val diffReportFillForward = leftReport.fillForwardBlankHeaders && rightReport.fillForwardBlankHeaders
    ReportContent(if (diffReportFillForward) removeHeaderDuplicates(rowsContent) else rowsContent,
      pivotHeaders(if (diffReportFillForward) removeHeaderDuplicates(colsContent) else colsContent),
      rows.map(row => cols.map(col => mainValueDiff(row, col))),
      rowColumnHeaders = pivotHeaders(if (diffReportFillForward) removeHeaderDuplicates(rowColumnHeadersContent) else rowColumnHeadersContent),
      fillForwardBlankHeaders = diffReportFillForward)
  }

  protected[tablediff] def removeDuplicateReportHeaders[R,C,M](report: ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]]) =
    report.copy(rowHeaders = removeHeaderDuplicates(report.rowHeaders),
      columnHeaders = pivotHeaders(removeHeaderDuplicates(pivotHeaders(report.columnHeaders))))

  protected[tablediff] def fillForwardReportHeaders[R,C,M](report: ReportContent[ValueDiff[R], ValueDiff[C], ValueDiff[M]]) =
    report.copy(rowHeaders = fillSectionHeaders(report.rowHeaders),
      columnHeaders = pivotHeaders(fillSectionHeaders(pivotHeaders(report.columnHeaders))))

  protected[tablediff] def zipLongestCommonSubsequence[T](leftSeq: ReportRow[T], rightSeq: ReportRow[T]): ReportRow[DiffLocation[T]] = {
    val seqLengths = Array.ofDim[Int](leftSeq.size + 1, rightSeq.size + 1)
    for ((left, i) <- leftSeq.zipWithIndex)
      for ((right, j) <- rightSeq.zipWithIndex)
        seqLengths(i + 1)(j + 1) =
          if (left == right)
            seqLengths(i)(j) + 1
          else
            math.max(seqLengths(i + 1)(j), seqLengths(i)(j + 1))

    @tailrec
    def diffWalk(accumulate: ReportRow[DiffLocation[T]], row: Int, col: Int): ReportRow[DiffLocation[T]] = {
      if (row == 0 && col == 0)
        accumulate
      else {
        val (nextDiff: DiffLocation[T], nextRow: Int, nextCol: Int) =
          if (row > 0 && col > 0 && leftSeq(row - 1) == rightSeq(col - 1)) {
            (DiffLocation(leftSeq(row - 1), Some(row - 1), Some(col - 1)), row - 1, col - 1)
          }
          else if (col > 0 && (row == 0 || seqLengths(row)(col - 1) >= seqLengths(row - 1)(col))) {
            (DiffLocation(rightSeq(col - 1), None, Some(col - 1)), row, col - 1)
          }
          else if (row > 0 && (col == 0 || seqLengths(row)(col - 1) < seqLengths(row - 1)(col))) {
            (DiffLocation(leftSeq(row - 1), Some(row - 1), None), row - 1, col)
          }
        diffWalk(nextDiff +: accumulate, nextRow, nextCol)
      }
    }
    diffWalk(emptyRow, leftSeq.size, rightSeq.size)
  }

//  case class Memoise[A, B](f: A => B) extends (A => B) {
//    private val cache = scala.collection.mutable.Map.empty[A, B]
//
//    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
//  }
//
//  This is a pretty algorithm, but I couldn't work out a way to make it tail recursive
//  def zipLongestCommonSubsequenceRec[T](leftSeq: IndexedSeq[(T, Int)], rightSeq: IndexedSeq[(T, Int)]): IndexedSeq[DiffLocation[T]] = {
//    lazy val zlcs: Memoise[(IndexedSeq[(T, Int)], IndexedSeq[(T, Int)]), IndexedSeq[DiffLocation[T]]] = Memoise {
//      case (left, IndexedSeq()) => left.map {
//        case (x, i) => DiffLocation(x, Some(i), None)
//      }
//      case (IndexedSeq(), right) => right.map {
//        case (x, i) => DiffLocation(x, None, Some(i))
//      }
//      case (left, right) if (left.head._1 == right.head._1) =>
//        DiffLocation(left.head._1, Some(left.head._2), Some(right.head._2)) +: zlcs(left.tail, right.tail)
//      case (left, right) =>
//        val rHeadless = zlcs(left, right.tail)
//        val lHeadless = zlcs(left.tail, right)
//        // version with the least Nones is the best
//        if (rHeadless.count(_.hasANone) >= lHeadless.count(_.hasANone))
//          DiffLocation(left.head._1, Some(left.head._2), None) +: lHeadless
//        else
//          DiffLocation(right.head._1, None, Some(right.head._2)) +: rHeadless
//    }
//    zlcs(leftSeq, rightSeq)
//  }

  def main(args: Array[String]) {
    zipLongestCommonSubsequence("Hello Bye".map(_ + ""), "xHell Bxet".map(_ + "")).foreach(println)
  }
}

