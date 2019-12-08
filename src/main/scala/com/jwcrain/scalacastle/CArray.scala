package com.jwcrain.scalacastle

import scala.annotation.tailrec

object CArray {
  def isSorted[T](array: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @tailrec def go(i: Int): Boolean = {
      if (i == array.length - 1) return true
      if (!ordered(array(i), array(i + 1))) return false
      go(i + 1)
    }

    go(0)
  }
}
