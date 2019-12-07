package com.jwcrain.scalacastle

import java.math.BigInteger

import scala.annotation.tailrec

object CMath {
  def factorial(n: Double): Double = {
    @tailrec def go(n: Double, accumulator: Double): Double = n match {
      case 0 => accumulator
      case _ => go(n - 1, accumulator * n)
    }
    
    go(n, 1.0)
  }

  def fibonacci(n: Double): Double = {
    @tailrec def go(i: Double, current: Double, previous: Double): Double = {
      if (n < 2) return n
      if (n == i) return current
      go(i + 1, current + previous, current)
    }

    go(0, 1, 0)
  }
}
