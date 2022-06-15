package convergence

import scala.math.abs

object Sequences {
  def f1(n:Int): Double = {
    val x = n.toDouble
    (3 * x*x - 4 * x + 1) / ( x*x - 8 * x + 15)
  }

  def f2(n:Int): Double = {
    val x = n.toDouble

    (3 * x*x - 4 * x + 1) / ( 2*x*x - 3 * x + 11)
  }

  def f3(n:Int): Double = {
    val x = n.toDouble

    (3 * x*x - 5 * x + 1) / ( 3*x*x - 5 * x - 1)
  }

  def approxLimit(f:Int=>Double, epsilon:Double):Double = {
    def recur(n:Int):Double = {
      val v1 = f(n)
      val v2 = f(n+1)
      if (abs(v1 - v2) <= epsilon)
        v2
      else
        recur(n+1)
    }
    recur(0)
  }

  def main(argv: Array[String]): Unit = {
    println(approxLimit(f1,0.01))
    println(approxLimit(f2,0.001))
    println(approxLimit(f3,0.0001))
  }
}
