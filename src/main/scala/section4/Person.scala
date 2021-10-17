package com.wentjiang
package section4

case class Person(name: Name, age: Age)
sealed class Name(val value:String)
sealed class Age(val value:Int)
object Person{
  def mkName (name:String): Either[String,Name] ={
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))
  }

  def mkName1 (name:String): Either[List[String],Name] ={
    if (name == "" || name == null) Left(List("Name is empty"))
    else Right(new Name(name))
  }

  def mkAge(age: Int):Either[String,Age] = {
    if (age<0) Left("Age is out of range")
    else Right(new Age(age))
  }


  def mkAge1(age: Int):Either[List[String],Age] = {
    if (age<0) Left(List("Age is out of range"))
    else Right(new Age(age))
  }

  def mkPerson(name:String,age:Int):Either[String,Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }

  def mkPerson2(name:String,age:Int):Either[List[String],Person] = {
    mkName1(name).map2_2[String,Age,Person](mkAge1(age))(Person(_,_))
  }

}