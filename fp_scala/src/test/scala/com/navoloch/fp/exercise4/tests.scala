package com.navoloch.fp.exercise4

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.util.{Success, Try}

@RunWith(classOf[JUnitRunner])
class tests extends FlatSpec with Matchers {

  case class Employee(name: String, department: String, manager: Option[String])

  def lookupByName(name: String): Option[Employee] = name match {
    case "Joe" => Some(Employee("Joe", "Finances", Some("Julie")))
    case "Mary" => Some(Employee("Mary", "IT", None))
    case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
    case _ => None
  }

  def getDepartment: (Option[Employee]) => Option[String] = _.map(_.department)
  def getManager: (Option[Employee]) => Option[String] = _.flatMap(_.manager)

  //<editor-fold desc="Exercise 4.1">

  getDepartment(lookupByName("Joe")) shouldBe Some("Finances")
  getDepartment(lookupByName("Mary")) shouldBe Some("IT")
  getDepartment(lookupByName("Foo")) shouldBe None

  getManager(lookupByName("Joe")) shouldBe Some("Julie")
  getManager(lookupByName("Mary")) shouldBe None
  getManager(lookupByName("Foo")) shouldBe None

  getManager(lookupByName("Joe")).orElse(Some("Mr. CEO")) shouldBe Some("Julie")
  getManager(lookupByName("Mary")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")
  getManager(lookupByName("Foo")).orElse(Some("Mr. CEO")) shouldBe Some("Mr. CEO")

  lookupByName("Joe").filter(_.department != "IT") shouldBe Some(Employee("Joe", "Finances", Some("Julie")))
  lookupByName("Mary").filter(_.department != "IT") shouldBe None
  lookupByName("Foo").filter(_.department != "IT") shouldBe None

  //</editor-fold>

  //<editor-fold desc="Exercise 4.2">

  //</editor-fold>

  //<editor-fold desc="Exercise 4.3">

  //</editor-fold>

  //<editor-fold desc="Exercise 4.4">

  Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  Option.sequence(List(Some(1), Some(2), None)) shouldBe None

  Option.sequence2(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  Option.sequence2(List(Some(1), Some(2), None)) shouldBe None
  //</editor-fold>

  //<editor-fold desc="Exercise 4.5">

  val list451 = List("1", "2", "3")
  val list452 = List("I", "II", "III", "IV")

  def parseInt(a: String): Option[Int] = Try(a.toInt) match {
    case Success(r) => Some(r)
    case _ => None
  }

  Option.traverse(list451)(i => parseInt(i)) shouldBe Some(List(1, 2, 3))
  Option.traverse(list452)(i => parseInt(i)) shouldBe None
  //</editor-fold>

  //<editor-fold desc="Exercise 4.6">

  def lookupByNameViaEither(name: String): Either[String, Employee] = name match {
    case "Joe" => Right(Employee("Joe", "Finances", Some("Julie")))
    case "Mary" => Right(Employee("Mary", "IT", None))
    case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
    case _ => Left("Employee not found")
  }

  def getDepartmentViaEither:(Either[String, Employee]) => Either[String, String] = _.map(e => e.department)

  def getManager(employee: Either[String, Employee]): Either[String, String] =
    employee.flatMap(e =>
      e.manager match {
        case Some(e) => Right(e)
        case _ => Left("Manager not found")
      })

  getDepartmentViaEither(lookupByNameViaEither("Joe")) shouldBe Right("Finances")
  getDepartmentViaEither(lookupByNameViaEither("Mary")) shouldBe Right("IT")
  getDepartmentViaEither(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")

  getManager(lookupByNameViaEither("Joe")) shouldBe Right("Julie")
  getManager(lookupByNameViaEither("Mary")) shouldBe Left("Manager not found")
  getManager(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")

  getManager(lookupByNameViaEither("Joe")).orElse(Right("Mr. CEO")) shouldBe Right("Julie")
  getManager(lookupByNameViaEither("Mary")).orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")
  getManager(lookupByNameViaEither("Foo")).orElse(Right("Mr. CEO")) shouldBe Right("Mr. CEO")
  //</editor-fold>

  //<editor-fold desc="Exercise 4.7">

  val employees = List("Joe", "Mary")
  val employeesAndOutsources = employees :+ "Foo"

  Either.traverse(employees)(lookupByNameViaEither) shouldBe Right(List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None)))
  Either.traverse(employeesAndOutsources)(lookupByNameViaEither) shouldBe Left("Employee not found")

  val employeesEither = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
  val employeesAndOutsourcesEither = employeesEither :+ lookupByNameViaEither("Foo")

  Either.sequence(employeesEither) shouldBe Right(List(Employee("Joe", "Finances", Some("Julie")), Employee("Mary", "IT", None)))
  Either.sequence(employeesAndOutsourcesEither) shouldBe Left("Employee not found")
  //</editor-fold>
}
