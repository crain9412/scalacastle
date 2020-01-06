package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class COptionTest extends FunSuite {
  test("COptionOf(x).getOrElse(-1) should equal x") {
    val option: COption[Int] = COptionOf(3)
    assert(option.getOrElse(() => -1) == 3)
  }

  test("COptionOf(2).map(_ * 2) should equal COptionOf(4)") {
    val option: COption[Int] = COptionOf(2)
    assert(option.map(_ * 2) == COptionOf(4))
  }

  test("COptionOf(2).flatMap(x => COptionOf(x * 2)) should equal COptionOf(4)") {
    val option: COption[Int] = COptionOf(2)
    assert(option.flatMap(x => COptionOf(x * 2)) == COptionOf(4))
  }

  test("COptionOf(5).filter(_ % 2 == 0).getOrElse(-1) should equal -1") {
    val option: COption[Int] = COptionOf(5)
    assert(option.filter(_ % 2 == 0).getOrElse(-1) == -1)
  }
}
