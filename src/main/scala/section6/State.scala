package com.wentjiang
package section6

case class State[S, +A](run: S => (A, S)) {
  //6.10
  def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = {
    s => {
      val (aValue, s1) = a(s)
      (f(aValue), s1)
    }
  }


}

object State {




  //6.10
  def unit[S, A](a: A): S => (A, S) = {
    s => (a, s)
  }

  //6.10
  def map2[S, A, B, C](a: S => (A, S))(b: S => (B, S))(f: (A, B) => C): S => (C, S) = {
    s => {
      val (aValue, s1) = a(s)
      val (bValue, s2) = b(s1)
      (f(aValue, bValue), s2)
    }
  }

  //6.11
  def flatMap[S, A, B](f: S => (A, S))(g: A => S => (B, S)): S => (B, S) = {
    s: S => {
      val (aValue: A, s1: S) = f(s)
      g(aValue)(s1)
    }
  }

  //6.11 ???
//  def sequence[S, A](fs: List[S => (A, S)]): S => (List[A], S) = {
//    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
//  }

}