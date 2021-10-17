package com.wentjiang
package section2

object Question2 {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  /**
   * 2.1
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, index: Int, num1: Int, acc: Int): Int = {
      if (index > n) {
        acc
      } else {
        go(n, index + 1, acc, num1 + acc)
      }
    }

    go(n, 1, 1, 0)
  }

  /**
   * 2.2
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    Vector.range(0, as.length).exists((index: Int) => index match {
      case num if num > 0 => ordered(as(index - 1), as(index))
      case _ => true
    })
  }

  /**
   * 2.3
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * 2.4
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * 2.5
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
//    val a: A => C = g andThen f
//    val b: A => C = f compose g
    g andThen f
  }
}
