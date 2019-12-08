package com.jwcrain.scalacastle

import scala.annotation.tailrec

object CFunctional {
  def multiApply[T](from: Int, to: Int, f: T => T, initialState: T): T = {
    @tailrec def go(i: Int, current: T): T = {
      if (i == to) return current
      go(i + 1, f(current))
    }

    go(from, initialState)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b => f(a, b))
}
