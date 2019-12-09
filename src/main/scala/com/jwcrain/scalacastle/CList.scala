package com.jwcrain.scalacastle

sealed trait CList[+T]
case object Nil extends CList[Nothing]
case class Node[+T](head: T, tail: CList[T]) extends CList[T]

object CList {
  def sum(ints: CList[Int]): Int = ints match {
    case Nil => 0
    case Node(head, tail) => head + sum(tail)
  }

  def product(doubles: CList[Double]): Double = doubles match {
    case Nil => 1.0
    case Node(head, tail) => head * product(tail)
  }

  def apply[T](nodes: T*): CList[T] = {
    if (nodes.isEmpty) Nil
    else Node(nodes.head, apply(nodes.tail: _*))
  }
}
