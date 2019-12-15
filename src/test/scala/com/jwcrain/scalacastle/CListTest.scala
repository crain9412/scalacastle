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

  test("tail of 1, 2, 3, 4, 5 should be 2, 3, 4, 5") {
    val expected: CList[Int] = CList(2, 3, 4, 5)
    assert(CList.tail(CList(1, 2, 3, 4, 5)) == expected )
  }

  test("dropping 3 elements from 1, 2, 3, 4, 5 should yield 4, 5") {
    val expected: CList[Int] = CList(4, 5)
    assert(CList.drop(CList(1, 2, 3, 4, 5), 3) == expected)
  }

  test("dropping even numbers from 6, 4, 2, 1, 2, 4, 6 should yield 1, 2, 4, 6") {
    val expected: CList[Int] = CList(1, 2, 4, 6)
    assert(CList.dropWhile(CList(6, 4, 2, 1, 2, 4, 6), (n: Int) => n % 2 == 0) == expected)
  }

  test("init 1, 2, 3 should yield 1, 2") {
    val expected: CList[Int] = CList(1, 2)
    assert(CList.init(CList(1, 2, 3)) == expected)
  }

  test("sumFold(1, 2, 3) should equal sum(1, 2, 3)") {
    val list: CList[Int] = CList(1, 2, 3)
    assert(CList.sum(list) == CList.sumFold(list))
  }

  test("product(4, 5, 6) should equal sumProduct(4, 5, 6)") {
    val list: CList[Double] = CList(4.0, 5.0, 6.0)
    assert(CList.product(list) == CList.productFold(list))
  }

  test("length of (5, 3, 1, 2) should be 4") {
    val list: CList[Int] = CList(5, 3, 1, 2)
    assert(CList.length(list) == 4)
  }

  test("foldLeft(1, 2, 3, 4, 5),2,(_ * _) should yield 240") {
    val list: CList[Int] = CList(1, 2, 3, 4, 5)
    assert(CList.foldLeft(2, list)(_ * _) == 240)
  }

  test("reverseFold(1, 2, 3) should yield (3, 2, 1)") {
    val expected: CList[Int] = CList(3, 2, 1)
    assert(CList.reverseFold(CList(1, 2, 3)) == expected)
  }

  test("append((1, 2, 3), 4) should yield (1, 2, 3, 4)") {
    val expected: CList[Int] = CList(1, 2, 3, 4)
    assert(CList.append(CList(1, 2, 3), 4) == expected)
  }

  test("map(1, 2, 3)(_ * 2) should yield (2, 4, 6)") {
    val expected: CList[Int] = CList(2, 4, 6)
    assert(CList.map(CList(1, 2, 3))(_ * 2) == expected)
  }

  test("filter(1, 2, 3, 4, 5)(_ % 2 == 0) should yield (2, 4)") {
    val expected: CList[Int] = CList(2, 4)
    assert(CList.filter(CList(1, 2, 3, 4, 5))(_ % 2 == 0) == expected)
  }

  test("flatMap(List(1,2,3))(i => List(i, i) should yield List(1,1,2,2,3,3)") {
    val expected: CList[Int] = CList(1, 1, 2, 2, 3, 3)
    assert(CList.flatMap(CList(1, 2, 3))(i => CList(i, i)) == expected)
  }

  test("filterWithFlatMap(1, 2, 3, 4, 5)(_ % 2 == 0) should yield (2, 4)") {
    val expected: CList[Int] = CList(2, 4)
    assert(CList.filterWithFlatMap(CList(1, 2, 3, 4, 5))(_ % 2 == 0) == expected)
  }
}
