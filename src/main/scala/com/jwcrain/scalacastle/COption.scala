package com.jwcrain.scalacastle

trait COption[+T] {
  def map[B](f: T => B): COption[B]

  def flatMap[B](f: T => COption[B]): COption[B]

  def getOrElse[B >: T](default: => B): B

  def orElse[B >: T](ob: => COption[B]): COption[B]

  def filter(f: T => Boolean): COption[T]

  def isEmpty: Boolean
}

case object EmptyOption extends COption[Nothing] {
  override def map[B](f: Nothing => B): COption[B] = this

  override def flatMap[B](f: Nothing => COption[B]): COption[B] = this

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => COption[B]): COption[B] = ob

  override def filter(f: Nothing => Boolean): COption[Nothing] = this

  override def isEmpty = true
}

case class COptionOf[T](t: T) extends COption[T] {
  def get: T = t

  override def isEmpty = false

  def apply(t: T): COption[T] = t match {
    case null => EmptyOption
    case _ => this
  }

  def map[B](f: T => B): COption[B] = {
    COptionOf(f(get))
  }

  def flatMap[B](f: T => COption[B]): COption[B] = {
    f(get)
  }

  def getOrElse[B >: T](default: => B): B = {
    get
  }

  def orElse[B >: T](ob: => COption[B]): COption[B] = {
    COptionOf(get)
  }

  def filter(f: T => Boolean): COption[T] = {
    if (!f(get)) return EmptyOption
    this
  }
}