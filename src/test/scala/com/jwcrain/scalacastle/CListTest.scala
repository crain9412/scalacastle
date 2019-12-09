package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CListTest extends FunSuite {
  test("1, 2, 3 should sum to 6") {
    val list: CList[Int] = CList(1, 2, 3)
    assert(CList.sum(list) == 6)
  }

  test("4, 5, 6 multiplied should be 120") {
    val list: CList[Double] = CList(4.0, 5.0, 6.0)
    assert(CList.product(list) == 120)
  }
}
