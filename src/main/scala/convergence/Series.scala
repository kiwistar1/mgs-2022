package convergence

object Series {

  import scala.math.{abs, pow}
  import functions.Factorial.factorial

  def exp(x: Double, epsilon: Double = 0.001): Double = {
    // sum up x^k / k! for k = 0 ... inf

    def recur(k: Int, sum: Double, term: Double): Double = {
      val term2 = term * x / k
      if (abs(term) <= epsilon)
        sum + term2
      else
        recur(k + 1, sum + term2, term2)
    }

    recur(1, 1.0, 1.0)
  }

  def sin(x: Double, epsilon: Double = 0.0001): Double = {
    // sum up (-1)^k * x^(2*k+1) / (2*k+1)!  for k = 0 ... inf

    def recur(k: Int, sum: Double, term: Double): Double = {
      val term2 = term * -1 * x * x / ((2 * k + 1) * (2 * k))
      if (abs(term2) < epsilon)
        sum + term2
      else
        recur(k + 1, sum + term2, term2)
    }

    recur(1, x, x)
  }

  def cos(x: Double, epsilon: Double = 0.0001): Double = {
    // sum up (-1^k) * x^(2k) / (2k)! for k = 0 ... inf
    def recur(k: Int, sum: Double, term: Double): Double = {
      val term2 = term * -1 * x * x / ((2 * k) * (2 * k - 1))
      if (abs(term2) <= epsilon)
        sum + term2
      else
        recur(k + 1, sum + term2, term2)
    }

    recur(1, 1.0, 1.0)
  }

  def main(argv: Array[String]): Unit = {
    println("exp = " + exp(1.0))
    println("exp = " + exp(1.0, 0.000001))

    println("sin = " + sin(3.14159 / 4))
    println("sin = " + sin(3.14159 / 2))
    println("cos = " + cos(3.14159 / 4))
    println("cos = " + cos(3.14159 / 2))
  }
}
