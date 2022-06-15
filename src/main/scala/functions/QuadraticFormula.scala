package functions

import scala.math.sqrt

// TASK:
//      Write a test to validate the following function.

import cats.instances.int
import scala.collection.View
object QuadraticFormula {

// the sign of the discriminant determines how many real roots the polynomial has
  def discriminant(a: Int, b: Int, c: Int): Int = {
    b * b - 4 * a * c
  }
  def quadraticFormula(a: Int, b: Int, c: Int): List[Double] = {
    val d = discriminant(a, b, c)
    // if there is 1 root, return it as a singleton list
    if (d == 0)
      List(-b / (2.0 * a))
    // if there are no roots, return an empty list
    else if (d < 0)
      List()
    // if there are 2 roots, return a list of them in increasing order
    else if (a < 0)
      List((-b + sqrt(d)) / (2.0 * a), (-b - sqrt(d)) / (2.0 * a))
    else
      List((-b - sqrt(d)) / (2.0 * a), (-b + sqrt(d)) / (2.0 * a))
  }

  // TASK:
  //    Update the formula to compute the discriminant,
  //    and detect whether there are zero, one, or two real roots.
  //    refactor the function to return a list of length 0, 1, or 2
  //    accordingly.  If there are two roots return them in increasing
  //    order.

  // TASK:
  //    Do your tests still work?  If not, do you need to
  //    update your tests, or do you need to update the function?
  //    Make sure your tests are up to date, and that they test the
  //    new features you implemented in previous step.

  def main(argv: Array[String]): Unit = {
    println("Hello this is main of QuadraticFormula")
  }
}
