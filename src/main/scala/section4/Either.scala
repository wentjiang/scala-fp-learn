package com.wentjiang
package section4



sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(error) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }
  }

  def map2_2[EE >: E, B, C](b: Either[List[EE], B])(f: (A, B) => C): Either[List[EE], C] = {
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e1: E), Left(e2: E)) => Left(List(e1,e2))
      case (Left(e), _) => Left(List(e))
      case (_, Left(e)) =>  Left(List(e).flatten)
    }
  }

  def map2_2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x=>x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h::t => (f(h).map2(traverse(t)(f))(_::_))
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

}
