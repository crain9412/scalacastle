package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CMathTest extends FunSuite {
  test("factorial of 4 should be 24") {
    assert(CMath.factorial(4) == 24.0)
  }
}
