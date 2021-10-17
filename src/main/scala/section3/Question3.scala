package com.wentjiang
package section3

import scala.::
import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }


  def question_3_1(): Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }

  /**
   * 3_2
   */
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  /**
   * 3_3
   */
  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => Cons(head, tail)
    }
  }

  /**
   * 3_4
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(_, tail) =>
        if (n > 1) {
          drop(tail, n - 1)
        } else {
          tail
        }
      case Nil => Nil
    }
  }

  /**
   * 3_5
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) =>
        if (f(head)) {
          dropWhile(tail, f)
        } else {
          Cons(head, dropWhile(tail, f))
        }
      case _ => l
    }
  }

  /**
   * 3_6
   */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(head, Cons(last, Nil)) => List(head)
      case Nil => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  /**
   * 3_7
   */
  def question_3_7() = {
    //see test
  }

  /**
   * 3_8
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
   * 3_9
   */
  def length[A](as: List[A]): Int = {
    val f: (A, Int) => Int = (_: A, length: Int) => length + 1
    List.foldRight(as, 0: Int)(f)
  }

  /**
   * 3_10 //todo use tail recursion
   */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  /**
   * 3_11
   */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x: Int, xs: List[Int]) => x + sum(xs)
  }

  /**
   * 3_11
   */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  /**
   * 3_11
   */
  def getLength[A](list: List[A]): Int = {
    val f: (Int, A) => Int = (length: Int, _: A) => length + 1
    foldLeft(list, 0)(f)
  }

  /**
   * 3_12
   */
  def reverse[A](list: List[A]): List[A] = {
    val newList: List[A] = Nil
    val f: (List[A], A) => List[A] = (list1: List[A], a: A) => Cons(a, list1)
    foldLeft(list, newList)(f)
  }

  /**
   * 3_13
   */
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val reverseList = reverse(as)
    foldLeft(reverseList,z)((b,a) => f(a,b))
  }

  /**
   * 3_14
   */
  def append[A](as: List[A],z:A): List[A] = {

  }
}
