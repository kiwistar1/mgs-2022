package functions

object Fibonacci {
  // TASK
  //    Implement fibonacci.   Given n, compute the nth Fibonacci number.
  def fibonacci(n: Int): Int = {
    assert(n>0)
    if (n == 1)
      1
    else if (n == 2)
      1
    else

      fibonacci(n - 1) + fibonacci(n - 2)
  }

  def main(argv: Array[String]): Unit = {
    for { n <- 1 to 20 } {
      println(s"$n --> ${fibonacci(n)}")
    }
  }
}
