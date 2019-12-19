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
      go(inputStream.tail(), CStream.node(inputStream.head(), currentStream))
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

  def foldRight[B](initial: B)(f: (T, B) => B): B = {
    this.foldRightHelper(this, initial)(f)
  }

  private def foldRightHelper[A, B](current: CStream[A], initial: B)(f: (A, B) => B): B = current match {
    case Empty => initial
    case StreamNode(hd, tl) => f(hd(), foldRightHelper(tl(), initial)(f))
  }

  def takeWhile(f: T => Boolean): CStream[T] = {
    @tailrec def go(inputStream: CStream[T], currentStream: CStream[T], i: Int): CStream[T] = {
      if (inputStream.isEmpty()) return currentStream
      if (!f(inputStream.head())) return currentStream
      go(inputStream.tail(), CStream.node(inputStream.head(), currentStream), i + 1)
    }

    go(this, CStream[T](), 0).reverse()
  }

  def map[A](f: T => A): CStream[A] = {
    foldRight(CStream[A]())((elementFromList, accumulator) => {
      CStream.node(f(elementFromList), accumulator)
    })
  }

  def forAll(f: T => Boolean): Boolean = {
    @tailrec def go(inputStream: CStream[T], currentStream: CStream[T], i: Int): Boolean = {
      if (inputStream.isEmpty()) return true
      if (!f(inputStream.head())) return false
      go(inputStream.tail(), CStream.node(inputStream.head(), currentStream), i + 1)
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

  def constant[T](a: T): CStream[T] = {
    node(a, constant(a))
  }

  def from(n: Int): CStream[Int] = {
    node(n, from(n + 1))
  }

  def empty[T]: CStream[T] = Empty

  def apply[T](stream: T*): CStream[T] =
    if (stream.isEmpty) empty else node(stream.head, apply(stream.tail: _*))
}