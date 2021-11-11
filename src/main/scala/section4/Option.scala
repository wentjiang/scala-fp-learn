package com.wentjiang
package section4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
   * 4.3
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  /*
  * 4.4
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap((hh: A) => sequence(t).map(hh :: _))
    }
  }

  def sequence_for[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => for {
        hh <- h
        a <- sequence_for[A](t).map(hh :: _)
      } yield a
    }
  }

  /**
   * 4.5
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  /**
   * 4.5.2
   */
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  }

}

