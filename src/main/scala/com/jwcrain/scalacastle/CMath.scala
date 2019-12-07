package com.jwcrain.scalacastle

import scala.annotation.tailrec

object CMath {
  def factorial(n: Double): Double = {
    @tailrec def go(n: Double, accumulator: Double): Double = n match {
      case 0 => accumulator
      case _ => go(n - 1, accumulator * n)
    }
    
    go(n, 1.0)
  }
}
