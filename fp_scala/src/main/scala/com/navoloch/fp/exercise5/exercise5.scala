package com.navoloch.fp

package object exercise5 {

  sealed trait Stream[+A] {

    //<editor-fold desc="Exercise 5.1">

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, tail) => h() :: tail().toList
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 5.2">

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case s => s
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 5.3">

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h())=> Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 5.4">

    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => true
    }
    //</editor-fold>

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    //<editor-fold desc="Exercise 5.5">

    def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => {
      if (p(h)) Stream.cons(h, t)
      else Stream.empty
    })
    //</editor-fold>

    //<editor-fold desc="Exercise 5.6">

    def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))
    //</editor-fold>

    //<editor-fold desc="Exercise 5.7">

    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons[B](f(h), t))

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => {
      if(f(h))
        Stream.cons[A](h, t.filter(f))
      else
        t.filter(f)
    })

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h) append t)
    //</editor-fold>

    //<editor-fold desc="Exercise 5.13">

    def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def takeViaUnfold(n: Int): Stream[A] = Stream.unfold(this) {
      case Cons(h, t) if n > 0 => Some(h(), t().take(n - 1))
      case _ => None
    }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Cons(h, t) if p(h())=> Some(h(), t().takeWhile(p))
      case _ => None
    }

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

    //<editor-fold desc="Exercise 5.19">


    //</editor-fold>

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    //<editor-fold desc="Exercise 5.8">

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
    //</editor-fold>

    //<editor-fold desc="Exercise 5.9">

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
    //</editor-fold>

    //<editor-fold desc="Exercise 5.10">

    def fibs: Stream[Int] = {
      def fib(f1: Int, f2: Int): Stream[Int] = Stream.cons[Int](f1, fib(f2, f1 + f2))
      fib(0, 1)
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 5.12">

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => Stream.cons(a, Stream.unfold(s)(f))
      case _ => Stream.empty[A]
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 5.12">

    def fibsViaUnfold: Stream[Int] = unfold((0, 1)) { case (f1, f2) => Some((f1, (f2, f1 + f2))) }

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n) {case i => Some(i, i + 1)}

    def constantViaUnfold(n: Int): Stream[Int] = unfold(n) {case i => Some(i, i)}

    def onesViaUnfold: Stream[Int] = unfold(1) {case _ => Some(1, 1)}
    //</editor-fold>

  }



}
