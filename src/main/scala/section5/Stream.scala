package com.wentjiang
package section5

import section5.Stream.cons


trait Stream[+A] {

  //5.1
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  //5.2
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), Stream.empty)
      case _ => Stream.empty
    }
  }

  //5.2
  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  //5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) =>
        cons(h(), t() takeWhile p)
      case _ => this
    }
  }

  //5.4
  def forAll1(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) if (p(h())) => true
      case Cons(_, t) => t().forAll1(p)
      case Empty => false
    }
  }

  //5.5
  def forAll2(p: A => Boolean): Boolean = {
    foldRight(true)((thisValue, previousValue) => previousValue && p(thisValue))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  //5.6
  def headOption1: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

  //5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((currentValue, previousValue) => cons(f(currentValue), previousValue))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((currentValue, previousValue) => if (f(currentValue)) {
      cons(currentValue, previousValue)
    } else {
      previousValue
    })
  }

  def append[B >: A](value: => Stream[B]): Stream[B] = {
    foldRight(value)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  //5.8
  def constant[A](a: A): Stream[A] = {
    Stream.cons[A](a, constant[A](a))
  }

  //5.9
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  //5.10
  def fib(num1: Int, num2: Int): Stream[Int] = {
    Stream.cons(num1, fib(num2, num2 + num1))
  }

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val (a, s) = f(z).get
    Stream.cons(a, unfold(s)(f))
  }

  //5.12
  def fibs_unfold(): Stream[Int] = {
    val f: ((Int, Int)) => Option[(Int, (Int, Int))] = (t: (Int, Int))
    => Option.apply((t._1, (t._2, t._1 + t._2)))
    unfold[Int, (Int, Int)]((0, 1))(f)
  }

  //5.12
  def from_unfold(n: Int): Stream[Int] = {
    val f: Int => Option[(Int, Int)] = (t: Int) => Option.apply[(Int, Int)]((t, t + 1))
    unfold[Int, Int](n)(f)
  }

  //5.12
  def constant_unfold(n: Int): Stream[Int] = {
    val f: Int => Option[(Int, Int)] = (t: Int) => Option.apply[(Int, Int)]((t, t))
    unfold(n)(f)
  }

  //5.12
  def ones_unfold(): Stream[Int] = {
    val f: Int => Option[(Int, Int)] = (t: Int) => Option.apply[(Int, Int)]((t, t))
    unfold(1)(f)
  }

  //5.13
  def map_unfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  //5.13
  def take_unfold(n: Int): Stream[A] = {
    unfold(this, n) {
      case (Cons(h, t), 1) => Some(h(), (Stream.empty[A], 0))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  //5.13
  def takeWhile_unfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }
  }

  //5.13
  def zipWith_unfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => {
        Some((f(h1(), h2()), (t1(), t2())))
      }
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)

  //5.14
  def startsWith[A](s:Stream[A]):Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  //5.15
  def tails:Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream.empty

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def main(args: Array[String]): Unit = {
    val x = Stream.cons[Int]({
      println("haha");
      2
    }, Stream.empty[Int])
    x.headOption
    x.headOption

  }

}