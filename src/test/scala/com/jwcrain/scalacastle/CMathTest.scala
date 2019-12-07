package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CMathTest extends FunSuite {
  test("factorial of 4 should be 24") {
    assert(CMath.factorial(4) == 24.0)
  }

  test("factorial of 1 should be 1") {
    assert(CMath.factorial(1) == 1.0)
  }

  test("factorial of 0 should be 1") {
    assert(CMath.factorial(0) == 1.0)
  }

  test("factorial of 14 should be 87178291200") {
    assert(CMath.factorial(14) == 87178291200.0)
  }

  test("the 20th fibonacci number should be 10946") {
    assert(CMath.fibonacci(20) == 10946.0)
  }
}
