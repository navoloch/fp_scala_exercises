package com.navoloch.fp.exercise5

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.util.{Success, Try}

@RunWith(classOf[JUnitRunner])
class tests extends FlatSpec with Matchers {

  //<editor-fold desc="Exercise 5.1">

  val s51 = Stream(1, 2, 3)
  s51.toList shouldBe List(1, 2, 3)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.2">

  val s52a = Stream(1, 2, 3)
  s52a.take(0).toList shouldBe Nil
  s52a.take(1).toList shouldBe List(1)
  s52a.take(2).toList shouldBe List(1, 2)
  s52a.take(3).toList shouldBe List(1, 2, 3)
  s52a.take(4).toList shouldBe List(1, 2, 3)

  val s52b = Stream(1, 2, 3, 4, 5)
  s52b.drop(0).toList shouldBe List(1, 2, 3, 4, 5)
  s52b.drop(1).toList shouldBe List(2, 3, 4, 5)
  s52b.drop(2).toList shouldBe List(3, 4, 5)
  s52b.drop(3).toList shouldBe List(4, 5)
  s52b.drop(4).toList shouldBe List(5)
  s52b.drop(5).toList shouldBe Nil
  s52b.drop(6).toList shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 5.3">

  val s53 = Stream(1, 2, 3, 4, 5)
  s53.takeWhile(_ < 3).toList shouldBe List(1, 2)
  s53.takeWhile(_ < 0).toList shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 5.4">

  Stream(1, 2, 3).forAll(_ % 2 == 0) shouldBe false
  Stream("a", "b", "c").forAll(_.nonEmpty) shouldBe true
  //</editor-fold>

  //<editor-fold desc="Exercise 5.5">

  val s55 = Stream(1, 2, 3, 4, 5)
  s55.takeWhileFold(_ < 3).toList shouldBe List(1, 2)
  s55.takeWhileFold(_ < 0).toList shouldBe Nil
  //</editor-fold>

  //<editor-fold desc="Exercise 5.6">

  Stream(1, 2, 3, 4, 5).headOption.getOrElse(0) shouldBe 1
  Stream().headOption.getOrElse(0) shouldBe 0
  //</editor-fold>

  //<editor-fold desc="Exercise 5.7">

  Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList shouldBe List(12, 14)
  Stream(1, 2, 3).flatMap(i => Stream(i + 1, i + 2)).toList shouldBe List(2, 3, 3, 4, 4, 5)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.8">

  Stream.constant(3).take(5).toList shouldBe List (3, 3, 3, 3, 3)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.9">

  Stream.from(100).take(5).toList shouldBe List (100, 101, 102, 103, 104)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.10">

  Stream.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.11">

  //</editor-fold>

  //<editor-fold desc="Exercise 5.12">

  Stream.fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  Stream.fromViaUnfold(100).take(5).toList shouldBe List (100, 101, 102, 103, 104)
  Stream.constantViaUnfold(3).take(5).toList shouldBe List (3, 3, 3, 3, 3)
  Stream.onesViaUnfold.take(5).toList shouldBe List (1, 1, 1, 1, 1)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.13">

  Stream(1, 2, 3, 4, 5).takeViaUnfold(0).toList shouldBe Nil
  Stream(1, 2, 3, 4, 5).takeViaUnfold(1).toList shouldBe List(1)
  Stream(1, 2, 3, 4, 5).takeViaUnfold(5).toList shouldBe List(1, 2, 3, 4, 5)
  //</editor-fold>

  //<editor-fold desc="Exercise 5.14">

  //</editor-fold>

  //<editor-fold desc="Exercise 5.15">

  //</editor-fold>

  //<editor-fold desc="Exercise 5.16">

  //</editor-fold>

  //<editor-fold desc="Exercise 5.17">

  //</editor-fold>

  //<editor-fold desc="Exercise 5.18">

  //</editor-fold>
}
