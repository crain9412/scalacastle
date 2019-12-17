package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CStreamTest extends FunSuite {
  test("CStream(1, 2, 3).toList should yield CList(1, 2, 3)") {
    val stream: CStream[Int] = CStream(1, 2, 3)
    val expected: CList[Int] = CList(1, 2, 3)
    assert(stream.toList == expected)
  }
}
