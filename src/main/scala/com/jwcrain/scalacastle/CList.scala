package com.jwcrain.scalacastle

import scala.annotation.tailrec

sealed trait CList[+T] {
  def head: T
  def tail: CList[T]
  def isEmpty: Boolean
}
case object Nil extends CList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  override def tail: Nothing = throw new NoSuchElementException("tail of empty list")
  override def isEmpty = true
}

case class Node[+T](hd: T, tl: CList[T]) extends CList[T] {
  override def head: T = hd
  override def tail: CList[T] = tl
  override def isEmpty = false
}

object CList {
  def sum(ints: CList[Int]): Int = ints match {
    case Nil => 0
    case Node(head, tail) => head + sum(tail)
  }

  def product(doubles: CList[Double]): Double = doubles match {
    case Nil => 1.0
    case Node(head, tail) => head * product(tail)
  }

  def tail[T](list: CList[T]): CList[T] = {
    if (list.isEmpty) Nil
    list.tail
  }

  def drop[T](list: CList[T], n: Int): CList[T] = {
    @tailrec def go(i: Int, currentList: CList[T]): CList[T] = {
      if (currentList.isEmpty) Nil
      if (i == n) return currentList
      go (i + 1, tail(currentList))
    }

    go(0, list)
  }

  def dropWhile[T](list: CList[T], f: T => Boolean): CList[T] = {
    @tailrec def go(currentList: CList[T]): CList[T] = {
      if (currentList.isEmpty) Nil
      if (!f(currentList.head)) return currentList
      go (tail(currentList))
    }

    go(list)
  }

  def push[T](list: CList[T], node: T): CList[T] = {
    Node(node, list)
  }

  def reverse[T](list: CList[T]): CList[T] = {
    if (list.isEmpty) Nil

    @tailrec def go(reversedList: CList[T], currentList: CList[T]): CList[T] = {
      if (currentList.isEmpty) return reversedList
      go(push(reversedList, currentList.head), tail(currentList))
    }

    go(Nil, list)
  }

  def init[T](list: CList[T]): CList[T] = {
    if (list.isEmpty) Nil

    @tailrec def go(createdList: CList[T], currentList: CList[T]): CList[T] = {
      if (currentList.isEmpty) Nil
      if (currentList.tail == Nil) return reverse(createdList)
      go(push(createdList, currentList.head), tail(currentList))
    }

    go(Nil, list)
  }

  def apply[T](nodes: T*): CList[T] = {
    if (nodes.isEmpty) Nil
    else Node(nodes.head, apply(nodes.tail: _*))
  }
}
