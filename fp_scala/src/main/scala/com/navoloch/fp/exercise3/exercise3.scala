package com.navoloch.fp

package object exercise3 {

  //<editor-fold desc="List">

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    //<editor-fold desc="Exercise 3.2">

    def tail[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(_, t) => t
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.3">

    def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, t) => Cons(newHead, t)
    }
    //</editor-fold>


    //<editor-fold desc="Exercise 3.4">

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case l => l
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.5">

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case l => l
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.6">

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
    //</editor-fold>


    //<editor-fold desc="Exercise 3.9">

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

    //</editor-fold>

    //<editor-fold desc="Exercise 3.10">

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.11">

    def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

    def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

    def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, h) => acc + 1)
    //</editor-fold>

    //<editor-fold desc="Exercise 3.12">

    def reverse[A](ns: List[A]) : List[A] = foldLeft(ns, List[A]())((acc, h) => Cons(h, acc))
    //</editor-fold>


    //<editor-fold desc="Exercise 3.13">

    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(l), z)((b,a) => f(a,b))
    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
    //</editor-fold>

    //<editor-fold desc="Exercise 3.14">

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def append2[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
    //</editor-fold>

    //<editor-fold desc="Exercise 3.15">

    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)
    //</editor-fold>

    //<editor-fold desc="Exercise 3.16">

    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    //</editor-fold>

    //<editor-fold desc="Exercise 3.17">
    def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    //</editor-fold>

    //<editor-fold desc="Exercise 3.18">
    def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
    //</editor-fold>

    //<editor-fold desc="Exercise 3.19">

    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)
    def removeOdds(l: List[Int]): List[Int] = filter(l)(h => h % 2 == 0)
    //</editor-fold>

    //<editor-fold desc="Exercise 3.20">

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
    //</editor-fold>

    //<editor-fold desc="Exercise 3.21">

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(e => if (f(e)) Cons(e, Nil: List[A]) else Nil: List[A])
    def removeOdds2(l: List[Int]): List[Int] = filter2(l)(h => h % 2 == 0)
    //</editor-fold>

    //<editor-fold desc="Exercise 3.22">

    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.23">

    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil: List[C]
      case (_, Nil) => Nil: List[C]
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.24">

    //</editor-fold>

    //<editor-fold desc="Exercise 3.25">

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.26">

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.27">

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    //</editor-fold>

    //<editor-fold desc="Exercise 3.28">

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 3.1">

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(n) => f(n)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    //</editor-fold>

    //</editor-fold>
}
