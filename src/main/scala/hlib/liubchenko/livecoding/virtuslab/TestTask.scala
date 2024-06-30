package hlib.liubchenko.livecoding.virtuslab

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TestTask extends AnyWordSpec with Matchers {
//  Simple task to implement
//    Given an input collection of tuples (id, year, profit), return a collection of tuples (id, year, profit, subtotal),
  //    sorted ascending by year, where subtotal is profit of current row + sum of profits of all previous years.
//
//    Example:
//    (1, 2015, 300),
//  (2, 2018, 150),
//  (3, 2014, 50),
//  (4, 2016, 100),
//  (5, 2017, 200)
//
//  Should return:
//  (3, 2014, 50, 50),
//  (1, 2015, 300, 350),
// ...

  case class Data(id: Int, year: Int, Profit: Long)
  case class CumulativeData(id: Int, year: Int, Profit: Long, cumulativeSum: Long)

  def countProfit_2(input: List[Data]): List[CumulativeData] = {
    var cumulativeSum = 0L
    input
      .sortBy(_.year)
      .map { case Data(id, year, profit) =>
        cumulativeSum += profit
        CumulativeData(id, year, profit, cumulativeSum)
      }
  }

  def countProfit(input: List[Data]): List[CumulativeData] =
    input
      .sortBy(_.year)
      .foldLeft(List.empty[CumulativeData]) {
        case (Nil, Data(id, year, profit)) => List(CumulativeData(id, year, profit, profit))
        case (acc, Data(id, year, profit)) => acc :+ CumulativeData(id, year, profit, acc.last.cumulativeSum + profit)
      }

  "countProfit" should {
    "work as expected" in {
      val input = List(
        (1, 2015, 300),
        (2, 2018, 150),
        (3, 2014, 50),
        (4, 2016, 100),
        (5, 2017, 200)
      ).map { case (id, year, profit) => Data(id, year, profit) }

      val expected = List(
        (3, 2014, 50, 50),
        (1, 2015, 300, 350),
        (4, 2016, 100, 450),
        (5, 2017, 200, 650),
        (2, 2018, 150, 800)
      ).map { case (id, year, profit, cumulativeProfit) => CumulativeData(id, year, profit, cumulativeProfit) }

      countProfit(input) shouldBe expected
    }
  }

  Future(println("a"))
    .flatMap(a =>
      Future(println("b"))
        .flatMap(b =>
          Future(println("c"))
            .map(c => a)
        )
    )

  Future(println("a"))
    .flatMap { a =>
      Future(println("b")).flatMap { b =>
        Future(println("c")).map(_ => a)
      }
    }

//  "Excercise: window functions
//
//  CREATE TABLE employees (
//    employee_id int,
//    employee_name string,
//    salary int,
//    department_id int,
//  );
//
//  CREATE TABLE departments (
//    department_id int,
//    department_name string
//  );
//
//  Create a query returning employees with highest salary in each department.

  // Select
  // window function partition by department_id order ask last(salary) as max_salary_per_dep
  // From employees as e
  // Join departments as d
  //  on e.department_id = d.department_id

}
