package com.navoloch.fp.exercise2

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class tests extends FlatSpec with Matchers {

  //<editor-fold desc="Exercise 2.1">

  fib(5) shouldEqual 5
  fib(0) shouldEqual 0
  fib(1) shouldEqual 1

  //</editor-fold>

  //<editor-fold desc="Exercise 2.2">

  isSorted(Array(1, 3, 5, 7), (x: Int, y: Int) => x > y) shouldBe true
  isSorted(Array(7, 5, 1, 3), (x: Int, y: Int) => x < y) shouldBe false
  isSorted(Array("Scala", "Exercises"), (x: String, y: String) => x.length > y.length) shouldBe true

  //</editor-fold>

  //<editor-fold desc="Exercise 2.3">

  curry(f)(1)(1) == f(1, 1) shouldBe true
  curry(f)(1)(1) == g(1)(1) shouldBe true
  //</editor-fold>

  //<editor-fold desc="Exercise 2.4">

  uncurry(g)(1, 1) == g(1)(1) shouldBe true
  uncurry(g)(1, 1) == f(1, 1) shouldBe true
  //</editor-fold>

  //<editor-fold desc="Exercise 2.5">

  compose(fc, gc)(0) == compose(gc, fc)(0) shouldBe false
  compose(fc, gc)(2) shouldBe 2
  compose(gc, fc)(2) shouldBe 3
  //</editor-fold>

}
