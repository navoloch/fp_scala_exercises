package com.navoloch.fp

package object exercise4 {

  //<editor-fold desc="Exercise 4.1">

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(s) => Some(f(s))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(s) => s
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = if (this.map(f).getOrElse(false)) this else None

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  //</editor-fold>

  object Option {
    //<editor-fold desc="Exercise 4.2">

    def variance(xs: Seq[Double]): Option[Double] = {
      None // WTF
    }
    //</editor-fold>

    //<editor-fold desc="Exercise 4.3">

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      l <- a
      r <- b
    } yield f(l, r)
    //</editor-fold>

    //<editor-fold desc="Exercise 4.4">

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h::t => h.flatMap(hh => sequence(t).map(hh::_))
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(e => e)
    //</editor-fold>

    //<editor-fold desc="Exercise 4.5">

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h::t => f(h).flatMap(hh => traverse(t)(f).map(hh::_))
    }
    //</editor-fold>
  }


  //<editor-fold desc="Exercise 4.6">

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }


    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(r) => Right(r)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      l <- this
      r <- b
    } yield f(l, r)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  //</editor-fold>

  object Either {
    //<editor-fold desc="Exercise 4.7">

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case h::t => h.flatMap(hh => sequence(t).map(hh::_))
    }

    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h::t => f(h).flatMap(hh => traverse(t)(f).map(hh::_))
    }
    //</editor-fold>

  }
}
