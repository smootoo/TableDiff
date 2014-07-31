package org.suecarter.tablediff

import scala.annotation.tailrec
import org.suecarter.tablediff.ReportContent._
import org.suecarter.tablediff.TableDiff.ValueDiff

/**
 * @param rowHeaders
 * @param columnHeaders
 * @param mainData
 * @param fillForwardBlankHeaders if repeating values in the header sections are left blank, fill forward the values
 *                                so that the produced diff has better context in header. Default true
 * @tparam R
 * @tparam C
 * @tparam M
 */
case class ReportContent[+R, +C, +M](rowHeaders: ReportSection[R] = emptySection,
                                     columnHeaders: ReportSection[C] = emptySection,
                                     mainData: ReportSection[M] = emptySection,
                                     rowColumnHeaders: ReportSection[C] = emptySection,
                                     fillForwardBlankHeaders: Boolean = true) {
  def this(r: Array[Array[R]], c: Array[Array[C]], m: Array[Array[M]], rch: Array[Array[C]]) =
    this(r.map(_.toSeq).toSeq, c.map(_.toSeq).toSeq, m.map(_.toSeq).toSeq, rch.map(_.toSeq).toSeq)

  // the shape of sections that need to be pivoted have to be like, (if not just square)
  // ****
  // ***
  // ***
  // *
  private def assertSectionIsTipTriangle[T](section: ReportSection[T], sectionName: String) {
    assert(section.sliding(2).collect{case Seq(l,r) => l.size >= r.size}.forall(x => x)
    , sectionName + " section needs to have same or decreasing number of elements in each row\n" +
        section.map(row => row.toString() + " " + row.size + " element" + (if (row.size>1) "s" else "")).mkString("\n") + "\n" +
        this)
  }
  assertSectionIsTipTriangle(rowColumnHeaders, "Top left")
  assertSectionIsTipTriangle(columnHeaders, "Top right")

  def isEmpty = rowWidth == 0 && columnCount == 0 && mainDataColumnCount == 0
  def nonEmpty = !isEmpty

  /**
   * how deep is the row section
   */
  val rowSectionWidth = (0 +: rowHeaders.map(_.size)).max

  /**
   * how deep are the row headers
   */
  val rowWidth = math.max((0 +: rowColumnHeaders.map(_.size)).max, rowSectionWidth)

  /**
   * how many rows
   */
  val rowCount = rowHeaders.size

  /**
   * how deep are the column headers
   */
  val columnDepth = math.max(columnHeaders.size,rowColumnHeaders.size)

  /**
   * how many main columns
   */
  val columnCount = (0 +: columnHeaders.map(_.size)).max

  /**
   * how many rows in the main data
   */
  val mainDataRows = mainData.size

  /**
   * how many columns in the main data
   */
  def mainDataColumnCount = (0 +: mainData.map(_.size)).max

  def mapAllCells[T >: Any, S](mapFunction: (T) => S) = {
    ReportContent[S, S, S](rowHeaders = rowHeaders.map(_.map(mapFunction)),
      columnHeaders = columnHeaders.map(_.map(mapFunction)),
      mainData = mainData.map(_.map(mapFunction)),
      rowColumnHeaders = rowColumnHeaders.map(_.map(mapFunction)))
  }

  def isEquivalent(rightReport: ReportContent[_,_,_]) = {
    ((rowWidth == 0 && rightReport.rowWidth == 0) || (rowHeaders == rightReport.rowHeaders && rowColumnHeaders == rightReport.rowColumnHeaders)) &&
    ((columnCount == 0 && rightReport.columnCount == 0) || columnHeaders == rightReport.columnHeaders) &&
    ((mainDataColumnCount == 0 && rightReport.mainDataColumnCount == 0) || mainData == rightReport.mainData)
  }
}

object ReportContent {
  type ReportSection[+T] = ReportRow[ReportRow[T]]
  type ReportRow[+T] = Seq[T]
  def emptyReport[T] = ReportContent[T, T, T]()
  def emptySection[T] = Seq[T]()
  def emptyRow[T] = Seq[T]()

  def apply[T](content: Array[Array[T]], rowWidth: Int, colDepth: Int) = {
    val (top, bottom) = content.map(_.toSeq).toSeq.splitAt(colDepth)
    val (topLeft, topRight) = top.map(_.splitAt(rowWidth)).unzip
    val (bottomLeft, bottomRight) = bottom.map(_.splitAt(rowWidth)).unzip
    new ReportContent[T, T, T](bottomLeft, topRight, bottomRight, topLeft)
  }

  def fillSectionHeaders[T](section: ReportSection[T]) = rowsProcessForward(section, fillRow[T])

  def removeHeaderDuplicates[T](section: ReportSection[ValueDiff[T]]) = rowsProcessForward[ValueDiff[T]](section, removeRowDuplicates[T])

  private def zipAllOption[T](left: ReportRow[T], right: ReportRow[T]) =
    left.map(Option(_)) zipAll (right.map(Option(_)), None, None)

  private val isEmpty = new PartialFunction[Any, Boolean] {
    def apply(o: Any) = {
      val trimmed = o match {
        case s: String => s.trim
        case c: Char => c.toString.trim
        case d if d == Right(None) => ""
        case x => x
      }
      isDefinedAt(trimmed) && {
        trimmed match {
          case a: AnyRef => a.getClass.getMethod("isEmpty").invoke(trimmed).asInstanceOf[Boolean]
        }
      }
    }

    def isDefinedAt(o: Any) = try {
      o match {
        case a: AnyRef => a.getClass.getMethod("isEmpty")
          true
        case _ => false
      }
    } catch {
      case _: Throwable => false
    }
  }
  
  private def fillRow[T](headRow: ReportRow[T], tailRow: ReportRow[T], originalHeadRow: ReportRow[Any]) = {
    val paddedZip = zipAllOption(headRow, tailRow)
    paddedZip.zipWithIndex.map {
      case ((head, tail), index) => {
        def emptyToLeft = paddedZip.take(index).forall{case(_, l) => l.map(isEmpty).getOrElse(true)}
        tail match {
          case Some(x) if isEmpty(x) && emptyToLeft => head
          case Some(_) => tail
          case None if emptyToLeft => head
          case None => None
        }
      }
    }.flatten
  }

  // only call the removal on a diff report result. This allows the use of Right(None) for an empty cell
  private def removeRowDuplicates[T](headRow: ReportRow[ValueDiff[T]],
                                     tailRow: ReportRow[ValueDiff[T]],
                                     originalHeadRow: ReportRow[ValueDiff[T]]): ReportRow[ValueDiff[T]] = {
    tailRow.zipWithIndex.map{case(tail, index) => {
      if (originalHeadRow.take(index+1) == tailRow.take(index+1)) Right(None) else tail
    }}
  }

  private def rowsProcessForward[T](section: ReportSection[T],
                                    rowFunction: (ReportRow[T], ReportRow[T], ReportRow[T]) => ReportRow[T]) = {
    @tailrec
    def processRowsForward(accumulator: ReportSection[T],
                           headRow: ReportRow[T],
                           originalHeadRow: ReportRow[T],
                           tailRows: ReportSection[T]): ReportSection[T] = {
      tailRows match {
        case Nil => accumulator :+ headRow
        case _ => processRowsForward(accumulator :+ headRow, rowFunction(headRow, tailRows.head, originalHeadRow), tailRows.head, tailRows.tail)
      }
    }
    section match {
      case Nil => section
      case _ => processRowsForward(emptyRow, section.head, section.head, section.tail)
    }
  }
}

