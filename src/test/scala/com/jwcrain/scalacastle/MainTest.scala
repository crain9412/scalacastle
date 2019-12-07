package com.jwcrain.scalacastle

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainTest extends FunSuite {
  test("main should exist") {
    val main = Main
    assert(main != null)
  }
}
