package functions

object ChallngingExercises2 {
  def pythagoreamtriple(m: Int, n: Int): List[List[Int]] = {
    for { b <- (m to n).toList
          a <-  (m to n).toList
            if (a < b)
          c <- (m to n).toList
          if (a*a + b*b == c*c)
               } 
               yield List(a, b, c)
              }
  def main(argv: Array[String]): Unit = {
    println(pythagoreamtriple(1, 1000))

  }
}
