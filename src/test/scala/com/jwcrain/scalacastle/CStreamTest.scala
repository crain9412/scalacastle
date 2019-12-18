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

  test("CStream(1, 2, 3, 4, 5).take(3).toList should yield CList(1, 2, 3)") {
    val stream: CStream[Int] = CStream(1, 2, 3, 4, 5)
    val expected: CList[Int] = CList(1, 2, 3)
    assert(stream.take(3).toList == expected)
  }

  test("CStream(1, 2, 3, 4, 5).drop(2).toList should yield CList(3, 4, 5)") {
    val stream: CStream[Int] = CStream(1, 2, 3, 4, 5)
    val expected: CList[Int] = CList(3, 4, 5)
    assert(stream.drop(2).toList == expected)
  }

  test("CStream(2, 4, 6, 8, 3, 2, 1).takeWhile(even).toList should yield CList(2, 4, 6, 8)") {
    val stream: CStream[Int] = CStream(2, 4, 6, 8, 3, 2, 1)
    val expected: CList[Int] = CList(2, 4, 6, 8)
    assert(stream.takeWhile(_ % 2 == 0).toList == expected)
  }
}
