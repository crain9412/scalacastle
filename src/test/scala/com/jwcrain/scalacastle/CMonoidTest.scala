package com.jwcrain.scalacastle

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CMonoidTest extends FunSuite {
  test("Test booleanOr's identity laws") {
    assert(CMonoid.booleanOr.zero || true)
    assert(!(CMonoid.booleanOr.zero || false))
  }

  test("Test booleanOr's associative laws") {
    val resultOne = CMonoid.booleanOr.op(true, CMonoid.booleanOr.op(false, false));
    val resultTwo = CMonoid.booleanOr.op(CMonoid.booleanOr.op(true, false), false);
    assert(resultOne == resultTwo)
  }

  test("Test booleanAnd's identity laws") {
    assert(CMonoid.booleanAnd.zero && true)
    assert(!(CMonoid.booleanAnd.zero && false))
  }
}
