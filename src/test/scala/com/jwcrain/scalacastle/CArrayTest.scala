package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CArrayTest extends FunSuite {
  test("1, 2, 3, 4, 5 should be sorted") {
    assert(CArray.isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))
  }

  test("1, 2, 3, 5, 4 should not be sorted") {
    assert(!CArray.isSorted(Array(1, 2, 3, 5, 4), (a: Int, b: Int) => a < b))
  }

  test("2, 1, 3, 4, 5 should not be sorted") {
    assert(!CArray.isSorted(Array(2, 1, 3, 4, 5), (a: Int, b: Int) => a < b))
  }
}
