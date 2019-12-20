package com.jwcrain.scalacastle


trait CMonoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object CMonoid {
  val intAddition: CMonoid[Int] = new CMonoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: CMonoid[Int] = new CMonoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 0
  }

  val booleanOr: CMonoid[Boolean] = new CMonoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: CMonoid[Boolean] = new CMonoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }
}

