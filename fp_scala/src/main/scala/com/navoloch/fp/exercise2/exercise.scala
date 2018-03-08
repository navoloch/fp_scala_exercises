package com.navoloch.fp

package object exercise2 {

  //<editor-fold desc="Exercise 2.1">

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n < 1)
        a
      else
        loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }
  //</editor-fold>

  //<editor-fold desc="Exercise 2.2">

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)
    go(0)
  }
  //</editor-fold>

  //<editor-fold desc="Exercise 2.3">

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
  def f(a: Int, b: Int): Int = a + b
  def g(a: Int)(b: Int): Int = a + b
  //</editor-fold>

  //<editor-fold desc="Exercise 2.4">

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  //</editor-fold>

  //<editor-fold desc="Exercise 2.5">

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
  def fc(b: Int): Int = b / 2
  def gc(a: Int): Int = a + 2
  //</editor-fold>

}
