package org.suecarter.tablediff

import org.scalatest.FunSuite
import org.suecarter.tablediff.ReportContent._
import scala.{List => L}

class ReportContentTests extends FunSuite {
  test("Check ReportContent validation") {
    val dodgySectionShape = L(L(1),L(1,2))
    intercept[AssertionError] {
      emptyReport.copy(columnHeaders = dodgySectionShape)
    }
    intercept[AssertionError] {
      emptyReport.copy(rowColumnHeaders = dodgySectionShape)
    }
    emptyReport.copy(rowHeaders = dodgySectionShape)
    emptyReport.copy(mainData = dodgySectionShape)
  }

  test("Is defined at") {
    assert(isEmpty(""))
    assert(isEmpty(" "))
    assert(isEmpty("x") === false)
    assert(isEmpty('x') === false)
    assert(isEmpty(' '))
    assert(isEmpty(None))
    assert(isEmpty(Right(None)))
    assert(isEmpty(Left(None)) === false)
    assert(isEmpty(List()))
    assert(isEmpty(List(1)) === false)
    assert(isEmpty(1l) === false)
  }
}
