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
}
