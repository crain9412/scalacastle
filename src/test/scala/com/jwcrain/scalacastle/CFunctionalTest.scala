package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CFunctionalTest extends FunSuite {
  test("addCurry(10)(10) should equal 20") {
    val addCurry = CFunctional.curry((a: Int, b:Int) => a + b)
    assert(addCurry(10)(10) == 20)
  }

  test("add(10, 10) should equal 20") {
    val addCurry = CFunctional.curry((a: Int, b:Int) => a + b)
    val add = CFunctional.decurry(addCurry)
    assert(add(10, 10) == 20)
  }

  test("minus5AndThenAdd10(10) should equal 15") {
    def add10(n: Int): Int = n + 10
    def minus5(n: Int): Int = n - 5
    val minus5andThenAdd10 = CFunctional.compose(add10, minus5)
    assert(minus5andThenAdd10(10) == 15)
  }
}
