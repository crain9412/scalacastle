package com.jwcrain.scalacastle
import scala.annotation.tailrec;

sealed trait CStream[+T] {
  def head: () => T
  def tail: () => CStream[T]
  def isEmpty: () => Boolean

  def toList: CList[T] = {
    @tailrec def go(currentStream: CStream[T], currentList: CList[T]): CList[T] = {
      if (currentStream.isEmpty()) return currentList
      go(currentStream.tail(), CList.append(currentList, currentStream.head()))
    }

    go(this, CList[T]())
  }

  def reverse(): CStream[T] = {
    @tailrec def go(inputStream: CStream[T], currentStream:CStream[T]): CStream[T] = {
      if (inputStream.isEmpty()) return currentStream
      go(inputStream.tail(), StreamNode(inputStream.head, () => currentStream))
    }

    go(this, CStream[T]())
  }

  def take(n: Int): CStream[T] = {
    @tailrec def go(inputStream: CStream[T], currentStream: CStream[T], i: Int): CStream[T] = {
      if (i == n) return currentStream
      if (inputStream.isEmpty()) return currentStream
      go(inputStream.tail(), CStream.node(inputStream.head(), currentStream), i + 1)
    }

    go(this, CStream[T](), 0).reverse()
  }

  def drop(n: Int): CStream[T] = {
    @tailrec def go(inputStream: CStream[T], currentStream: CStream[T], i: Int): CStream[T] = {
      if (i == n + 1) return currentStream
      if (inputStream.isEmpty()) return currentStream
      go(inputStream.tail(), CStream.node(inputStream.head(), inputStream.tail()), i + 1)
    }

    go(this, CStream[T](), 0)
  }
}
case object Empty extends CStream[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head of empty stream")
  override def tail: Nothing = throw new NoSuchElementException("tail of empty stream")
  override def isEmpty: () => Boolean = () => true
}

case class StreamNode[T](hd: () => T, tl: () => CStream[T]) extends CStream[T] {
  override def head: () => T = hd
  override def tail: () => CStream[T] = tl
  override def isEmpty: () => Boolean = () => false
}

object CStream {
  def node[T](hd: => T, tl: => CStream[T]): CStream[T] = {
    lazy val head = hd
    lazy val tail = tl
    StreamNode(() => head, () => tail)
  }

  def empty[T]: CStream[T] = Empty

  def apply[T](stream: T*): CStream[T] =
    if (stream.isEmpty) empty else node(stream.head, apply(stream.tail: _*))
}