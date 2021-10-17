package com.wentjiang
package section2

class Question2Test extends org.specs2.mutable.Specification {

  "factorial" in {
    Question2.factorial(3) mustEqual (6)
  }

  "fib" in {
    Question2.fib(1) mustEqual (1)
    Question2.fib(2) mustEqual (1)
    Question2.fib(3) mustEqual (2)
    Question2.fib(4) mustEqual (3)
    Question2.fib(5) mustEqual (5)
    Question2.fib(6) mustEqual (8)
  }

  val stringOrder: (String, String) => Boolean = (str1:String, str2:String) =>{
    str1.compareTo(str2) > 0
  }

  val intOrder: (Int, Int) => Boolean = (num1: Int, num2: Int) => {
    num2 >= num1
  }

  "isSorted" in {
    Question2.isSorted[Int](Array(1, 2, 3, 4, 5), intOrder) must beEqualTo(true)

    Question2.isSorted[Int](Array(1, 2, 4, 3, 5), intOrder) must beEqualTo(true)

    Question2.isSorted[String](Array("a","b","c","d","e"),stringOrder)
  }
}
