package com.navoloch.fp.exercise3

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class tests extends FlatSpec with Matchers {

  //<editor-fold desc="Exercise 3.1">


  //</editor-fold>

  //<editor-fold desc="Exercise 3.2">

  tail(List(1, 2, 3)) shouldBe List(2, 3)
  tail(List(1)) shouldBe Nil
  tail(Nil) shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 3.3">

  setHead(List(1, 2, 3), 3) shouldBe List(3,2,3)
  setHead(List("a", "b"), "c") shouldBe List("c", "b")
  //</editor-fold>

  //<editor-fold desc="Exercise 3.4">

  drop(List(1, 2, 3), 1) shouldBe List(2, 3)
  drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
  drop(List("a", "b"), 2) shouldBe Nil
  drop(List(1, 2), 3) shouldBe Nil
  drop(Nil, 1) shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 3.5">

  dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldBe List(2, 3)
  dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldBe List(1, 2, 3)
  dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldBe Nil
  dropWhile(Nil, (x: Int) => x > 0) shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 3.6">

  init(List(1, 2, 3)) shouldBe List(1, 2)
  init(List(1)) shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 3.7">


  //</editor-fold>

  //<editor-fold desc="Exercise 3.8">

  foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1,2,3)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.9">

  com.navoloch.fp.exercise3.length(List(1, 2, 3, 4, 5)) shouldBe 5
  //</editor-fold>

  //<editor-fold desc="Exercise 3.10">


  //</editor-fold>

  //<editor-fold desc="Exercise 3.11">

  def listInts = List(1, 2, 3, 4, 5)
  def listDoubles = List(1.0, 2.0, 3.0)
  sum3(listInts) shouldBe 15
  product3(listDoubles) shouldBe 6.0
  length2(listInts) shouldBe 5
  //</editor-fold>

  //<editor-fold desc="Exercise 3.12">


  //</editor-fold>

  //<editor-fold desc="Exercise 3.13">


  //</editor-fold>

  //<editor-fold desc="Exercise 3.14">

  append2(List(1, 2, 3), List(1, 2)) shouldBe List(1, 2, 3, 1, 2)
  append2(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
  append2(Nil, List(1, 2)) shouldBe List(1, 2)
  append2(Nil, Nil) shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 3.15">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.16">

  add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.17">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.18">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.19">

  removeOdds(List(1, 2, 3, 4, 5)) shouldBe List(2, 4)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.20">

  flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3,  3)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.21">

  removeOdds2(List(1, 2, 3, 4, 5)) shouldBe List(2, 4)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.22">

  addPairwise(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  //</editor-fold>

  //<editor-fold desc="Exercise 3.23">

  zipWith(List("a", "b", "c"), List("A", "B", "C"))(_ + _) shouldBe List("aA", "bB", "cC")
  zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString()) shouldBe List("14", "25", "36")
  //</editor-fold>

  //<editor-fold desc="Exercise 3.24">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.25">

  def t25 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  com.navoloch.fp.exercise3.size(t25) shouldBe 5
  //</editor-fold>

  //<editor-fold desc="Exercise 3.26">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.27">

  //</editor-fold>

  //<editor-fold desc="Exercise 3.28">

  def t28 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  com.navoloch.fp.exercise3.map(t28)(_ * 2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  //</editor-fold>

  //<editor-fold desc="Exercise 3.29">
  def t29 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  sizeViaFold(t29) shouldBe 5
  maximumViaFold(t29) shouldBe 3
  depthViaFold(t29) shouldBe 2
  mapViaFold(t29)(_ % 2 == 0) shouldBe
  Branch(Branch(Leaf(false), Leaf(true)), Leaf(false))
  //</editor-fold>


}
