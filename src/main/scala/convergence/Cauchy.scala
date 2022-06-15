package convergence

import scala.math.sin

object Cauchy {
  def f(x: Double): Double = {
    (3 * x * x - 4 * x + 1) / ( x * x - 8 * x + 15)
  }

  def g(x:Double):Double = {
    2 * sin(2*x*x + 3*x - 1)
  }

  def main(argv: Array[String]): Unit = {
    for {x <- 0 to 1000000
         v = g(x) // or f(x)
         } println(s"$x -> $v")
  }
}
