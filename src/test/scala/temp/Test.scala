package com.wentjiang
package temp

object Test {
  def main(args: Array[String]): Unit = {
    val result: Result[Int] = Result(42)
    for {
      res <- result
    } println(res)

    for {
      res <- result
    } yield res * 2

    val anotherResult: Result[Int] = Result(100)
    for {
      res <- result
      another <- anotherResult
    } yield res + another
  }

}


case class Result[A](result: A) {
  def foreach(f: A => Unit): Unit = f(result)

  def map[B](f: A => B): Result[B] = Result(f(result))

  def flatMap[B](f: A => Result[B]): Result[B] = f(result)
}