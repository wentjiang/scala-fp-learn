package com.wentjiang
package section3

import org.specs2.mutable.Specification

class Question3Test extends Specification {

  "Question3Test" should {
    "question_3_1" in {
      List.question_3_1() mustEqual (3)
    }

    "question_3_2" in {
      val list = List(1, 2, 3, 4)
      List.tail(list) mustEqual (List(2, 3, 4))
    }

    "question_3_3" in {
      val list = List(1, 2, 3, 4)
      List.setHead(list, 2) mustEqual (List(2, 2, 3, 4))
    }

    "question_3_4 case 1" in {
      val list = List(1, 2, 3, 4)
      List.drop(list, 2) mustEqual (List(3, 4))
    }

    "question_3_4 case 2" in {
      val list = List(1, 2, 3, 4, 4, 5)
      List.drop(list, 3) mustEqual (List(4, 4, 5))
    }

    "question_3_5" in {
      val list = List(1, 2, 3, 4, 5, 6)
      val f: (Int) => Boolean = (num) => {
        num % 2 == 0
      }
      List.dropWhile(list, f) mustEqual List(1, 3, 5)
    }

    "question_3_6" in {
      val list = List(1, 2, 3, 4, 5, 6)
      List.init(list) mustEqual List(1, 2, 3, 4, 5)
    }

    "question_3_7" in {
      val list = List(1, 2, 3, 0, 4, 5, 6)
      List.foldRight(list, 0.0)(_ * _) mustEqual (0.0)
    }

    "question_3_8" in {
      val list = List(1, 2, 3, 4)
      List.foldRight(list, Nil: List[Int])(Cons(_, _)) mustEqual List(1, 2, 3, 4)
    }

    "question_3_9" in {
      val list = List(1, 2, 3, 4)
      List.length(list) mustEqual 4
    }

    "question_3_11" in {
      val list = List(1, 2, 3, 4)
      List.getLength(list) mustEqual 4
    }

    "question_3_12" in {
      val list = List(1, 2, 3, 4)
      List.reverse(list) mustEqual List(4, 3, 2, 1)
    }


  }

}
