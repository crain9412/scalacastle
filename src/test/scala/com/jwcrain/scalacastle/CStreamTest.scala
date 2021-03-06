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

  test("CStream(1, 3, 5, 7).forAll(even) should be true") {
    val stream: CStream[Int] = CStream(1, 3, 5, 7)
    assert(stream.forAll(_ % 2 != 0))
  }

  test("CStream(1, 3, 5, 7, 8).forAll(even) should be false") {
    val stream: CStream[Int] = CStream(1, 3, 5, 7, 8)
    assert(!stream.forAll(_ % 2 != 0))
  }

  test("CStream(1, 2, 3).foldRight(3)((x, y) => x + y) should yield 9") {
    val stream: CStream[Int] = CStream(1, 2, 3)
    val expected: Int = 9
    assert(stream.foldRight(3)(_ + _) == expected)
  }

  test("I should be able to take 5 elements of an infinite stream") {
    lazy val ones: CStream[Int] = CStream.constant(1)
    val expected: CList[Int] = CList(1, 1, 1, 1, 1)
    assert(ones.take(5).toList == expected)
  }

  test("CStream(6, 8, 10).map(_ / 2).toList should yield CList(3, 4, 5)") {
    val stream: CStream[Int] = CStream(6, 8, 10)
    val expected: CList[Int] = CList(3, 4, 5)
    assert(stream.map((a: Int) => {a / 2}).toList == expected)
  }

  test("I can count to five with an infinite stream") {
    lazy val incrementor: CStream[Int] = CStream.from(1)
    val expected: CList[Int] = CList(1, 2, 3, 4, 5)
    assert(incrementor.take(5).toList == expected)
  }

  test("Test some exponent unfolding") {
    val currentState: Int = 1
    def exponentialWithoutOverflow(n: Int): Option[(Int, Int)] = {
      if (n >= Int.MaxValue / 2) return None
      Some((n, n * 2))
    }
    val expected: CList[Int] = CList(1, 2, 4, 8, 16)
    assert(CStream.unfold(currentState)(exponentialWithoutOverflow).take(5).toList == expected)
  }
}
